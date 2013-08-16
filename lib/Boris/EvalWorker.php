<?php

/* vim: set shiftwidth=2 expandtab softtabstop=2: */

namespace Boris;

/**
 * EvalWorker is responsible for evaluating PHP expressions in forked processes.
 */
class EvalWorker {
  const ABNORMAL_EXIT = 255;
  const DONE   = "\0";
  const EXITED = "\1";
  const FAILED = "\2";
  const READY  = "\3";
  const RESPONSE = "\4";

  private $_socket;
  private $_server_socket;
  private $_clients = array();
  private $_exports = array();
  private $_startHooks = array();
  private $_failureHooks = array();
  private $_ppid;
  private $_pid;
  private $_cancelled;
  private $_inspector;
  private $_exceptionHandler;
  private $_completionParser;

  /**
   * Create a new worker using the given socket for communication.
   *
   * @param resource $socket
   */
  public function __construct($socket, $port) {
    $this->_socket    = $socket;
    $this->_inspector = new DumpInspector();
    stream_set_blocking($socket, 0);

    if(!empty($port)) {
      $this->_server_socket = stream_socket_server("tcp://127.0.0.1:$port", $errno, $errstr);
      if(!$this->_server_socket) {
        print "Error starting server on port $port: '$errstr' ($errno)\n";
      } else {
        print "Listening on port $port.\n";
      }
    }

    $this->_completionParser = new CompletionParser();
  }

  /**
   * Set local variables to be placed in the workers's scope.
   *
   * @param array|string $local
   * @param mixed $value, if $local is a string
   */
  public function setLocal($local, $value = null) {
    if (!is_array($local)) {
      $local = array($local => $value);
    }

    $this->_exports = array_merge($this->_exports, $local);
  }

  /**
   * Set hooks to run inside the worker before it starts looping.
   *
   * @param array $hooks
   */
  public function setStartHooks($hooks) {
    $this->_startHooks = $hooks;
  }

  /**
   * Set hooks to run inside the worker after a fatal error is caught.
   *
   * @param array $hooks
   */
  public function setFailureHooks($hooks) {
    $this->_failureHooks = $hooks;
  }

  /**
   * Set an Inspector object for Boris to output return values with.
   *
   * @param object $inspector any object the responds to inspect($v)
   */
  public function setInspector($inspector) {
    $this->_inspector = $inspector;
  }

  /**
   * Start the worker.
   *
   * This method never returns.
   */
  public function start() {
    $__scope = $this->_runHooks($this->_startHooks);
    $this->_write($this->_socket, self::READY);

    for (;;) {
      declare(ticks = 1);
      // don't exit on ctrl-c
      pcntl_signal(SIGINT, SIG_IGN, true);

      $this->_cancelled = false;

      list($input, $client) = $this->_read($this->_socket);

      if ($input === null) {
        continue;
      }

      switch ($input->operation) {
      case 'evaluate':
        /* Expression evaluation */
        $__response = $this->_doEval($input->statement, $__scope);
        break;

      case 'complete':
        $return = $this->_doComplete($input->line, $__scope);
        $__response = $this->_packResponse($return);
        break;

      case 'hint':
        $return = $this->_doHint($input->line, $__scope);
        $__response = $this->_packResponse($return);
        break;

      case 'documentation':
        $return = $this->_doDocumentation($input->line, $__scope);
        $__response = $this->_packResponse($return);
        break;

      default:
        throw new \RuntimeException(sprintf("Bad operation '%s'", $input->operation));
        $__response = self::DONE;
      }

      $this->_write($client, $__response);
      
      if ($__response == self::EXITED) {
        exit(0);
      }
    }
  }

  /**
   * While a child process is running, terminate it immediately.
   */
  public function cancelOperation() {
    printf("Cancelling...\n");
    $this->_cancelled = true;
    posix_kill($this->_pid, SIGKILL);
    pcntl_signal_dispatch();
  }

  /**
   * If any user-defined exception handler is present, call it, but be sure to exit correctly.
   */
  public function delegateExceptionHandler($ex) {
    call_user_func($this->_exceptionHandler, $ex);
    exit(self::ABNORMAL_EXIT);
  }

  // -- Private Methods

  private function _runHooks($hooks) {
    extract($this->_exports);

    foreach ($hooks as $__hook) {
      if (is_string($__hook)) {
        eval($__hook);
      } elseif (is_callable($__hook)) {
        call_user_func($__hook, $this, get_defined_vars());
      } else {
        throw new \RuntimeException(
          sprintf(
            'Hooks must be closures or strings of PHP code. Got [%s].',
            gettype($__hook)
          )
        );
      }

      // hooks may set locals
      extract($this->_exports);
    }

    return get_defined_vars();
  }

  function _doEval($__input, &$__scope) {
    $__response = self::DONE;

    $this->_ppid = posix_getpid();
    $this->_pid  = pcntl_fork();

    if ($this->_pid < 0) {
      throw new \RuntimeException('Failed to fork child labourer');
    } elseif ($this->_pid > 0) {
      // kill the child on ctrl-c
      pcntl_signal(SIGINT, array($this, 'cancelOperation'), true);
      pcntl_waitpid($this->_pid, $__status);

      if (!$this->_cancelled && $__status != (self::ABNORMAL_EXIT << 8)) {
        $__response = self::EXITED;
      } else {
        $this->_runHooks($this->_failureHooks);
        $__response = self::FAILED;
      }
    } else {
      // user exception handlers normally cause a clean exit, so Boris will exit too
      if (!$this->_exceptionHandler =
          set_exception_handler(array($this, 'delegateExceptionHandler'))) {
        restore_exception_handler();
      }

      // undo ctrl-c signal handling ready for user code execution
      pcntl_signal(SIGINT, SIG_DFL, true);
      $__pid = posix_getpid();

      $__result = $this->_evalInScope($__input, $__scope);

      if (posix_getpid() != $__pid) {
        // whatever the user entered caused a forked child
        // (totally valid, but we don't want that child to loop and wait for input)
        exit(0);
      }

      if (preg_match('/\s*return\b/i', $__input)) {
        fwrite(STDOUT, sprintf(" → %s\n", $this->_inspector->inspect($__result)));
      }
      $this->_expungeOldWorker();
    }
    return $__response;
  }

  private function _doComplete($input, $scope) {
    $info = $this->_completionParser->getCompletionInfo($input);
    if($info === NULL) return NULL;

    switch($info->how) {
      /* FIXME: Constants and properties are case sensitive. Functions
       * and methods (normal or static) are not. */
    case CompletionParser::COMPLETE_MEMBER:
      $context = $this->_getCompletionContext($info, $scope);
      $candidates = $this->_objectMembers($context);
      $completions = $this->_filterCompletions($candidates, $info->symbol);
      break;

    case CompletionParser::COMPLETE_STATIC:
      $context = $this->_getCompletionContext($info, $scope);
      $candidates = $this->_staticMembers($context);
      $completions = $this->_filterCompletions($candidates, $info->symbol);
      break;
      
    case CompletionParser::COMPLETE_VARIABLE:
      $candidates = array_map(function($name) { return '$' . $name; },
                              array_keys($scope));
      $completions = $this->_filterCompletions($candidates, $info->symbol);

      break;

    case CompletionParser::COMPLETE_INDEX:
      $context = $this->_getCompletionContext($info, $scope);
      if(!is_array($context)) {
        $completions = array();
      } else {
        $completions = $this->_filterCompletions(array_keys($context),
                                                 $info->symbol);
      }
      break;

    case CompletionParser::COMPLETE_CLASS:
      $this->_stripInitialSlash($info);
      $completions = $this->_filterCompletions(get_declared_classes(),
                                               $info->symbol, TRUE);
      $completions = array_map(function($name) { return $name . '('; },
                               $completions);
      break;

    case CompletionParser::COMPLETE_SYMBOL:
      $this->_stripInitialSlash($info);
      /* Constants are case sensitive, but other names are not. */
      $constants = $this->_filterCompletions(array_keys(get_defined_constants()),
                                             $info->symbol, FALSE);
      $symbols = $this->_filterCompletions($this->_bareSymbols(),
                                           $info->symbol, TRUE);
      $completions = array_merge($constants, $symbols);
      break;

    default:
      throw new \RuntimeException(sprintf(
        "Unexpected value %s returned from getCompletionInfo",
        $info->how));
    }

    return array('start' => $info->start,
                 'end' => $info->end,
                 'completions' => $completions);
  }

  /* Handle an annoying special case with absolutely qualified names */
  private function _stripInitialSlash(&$info) {
    if($info->symbol[0] == '\\') {
      $info->start += 1;
      $info->symbol = substr($info->symbol, 1);
    }
  }

  private function _packResponse($response) {
    $serialized = json_encode($response);
    return self::RESPONSE . pack('N', strlen($serialized)) . $serialized;
  }

  private function _getReflectionObject($line, $scope) {
    $info = $this->_completionParser->getDocInfo($line);
    if(!$info) return NULL; 
    
    try {
      switch($info[0]) {
      case CompletionParser::FUNCTION_INFO:
        return new \ReflectionFunction($info[1]);
        break;

      case CompletionParser::CLASS_INFO:
        try {
          return new \ReflectionMethod($info[1], '__construct');
        } catch (\ReflectionException $e) {
          return new \ReflectionMethod($info[1], $info[1]);
        }
        break;

      case CompletionParser::METHOD_INFO:
        list(, $base, $bare, $method) = $info;
        if($bare) $obj = $base;
        else $obj = $this->_evalInScope("return $base;", $scope);
        return new \ReflectionMethod($obj, $method);
        break;

      default:
        throw new \RuntimeException(
          sprintf("Unexpected code %s from CompletionParser::getDocInfo",
                  $info[0]));
      }
    } catch (\ReflectionException $e) {
      return NULL;
    }
  }

  private function _doHint($line, $scope) {
    $refl = $this->_getReflectionObject($line, $scope);
    if(!$refl) return NULL;
    try { 
      return $this->_describeFunctionOrMethod($refl);
    } catch(\ReflectionException $e) {
      return NULL;
    }
  }

  private function _doDocumentation($line, $scope) {
    $refl = $this->_getReflectionObject($line, $scope);
    if(!$refl) return NULL;
    try { 
      return $refl->__toString();
    } catch(\ReflectionException $e) {
      return NULL;
    }
  }

  private function _describeFunctionOrMethod($refl) {
    $params = $refl->getParameters();
    $required = array_splice($params, 0, $refl->getNumberOfRequiredParameters());
    $arg_string = $this->_describeParams($required);
    if(count($params)) {
      $arg_string .= sprintf(' [, %s ]', $this->_describeParams($params));
    }
    
    return sprintf('%s%s ( %s )',
                   $refl->returnsReference() ? '&' : '',
                   ($refl->name == '__construct') ? $refl->getDeclaringClass()->name : $refl->name,
                   $arg_string);
  }

  private function _describeParams($params) {
    return implode(', ', array_map(function($param) {
      $base = sprintf('%s$%s',
                      $param->isPassedByReference() ? '&' : '',
                      $param->name);
      if($param->isDefaultValueAvailable())
        return $base . " = " . var_export($param->getDefaultValue(), TRUE);
      else
        return $base;
    }, $params));
  }

  private function _evalInScope($__boris_code, &$__boris_scope) {
    extract($__boris_scope);
    $__boris_result = eval($__boris_code);
    $__boris_scope = array_diff_key(get_defined_vars(), array(
      '__boris_code' => 1,
      '__boris_scope' => 1,
      '__boris_result' => 1));
    return $__boris_result;
  }

  private function _expungeOldWorker() {
    posix_kill($this->_ppid, SIGTERM);
    pcntl_signal_dispatch();
  }

  private function _write($socket, $data) {
    $total = strlen($data);
    for ($written = 0; $written < $total; $written += $fwrite) {
      $fwrite = fwrite($socket, substr($data, $written));
      if ($fwrite === false) {
        throw new \RuntimeException(
          sprintf('Socket error: wrote only %d of %d bytes.',
                  $written, $total));
      }
    }
  }

  private function _read($socket)
  {
    $sockets = $this->_clients;
    $sockets[] = $socket;
    if($this->_server_socket)
      $sockets[] = $this->_server_socket;

    $write = NULL;
    $read = $except = $sockets;

    if (stream_select($read, $write, $except, 10) > 0) {
      if ($read) {
        foreach ($read as $reader) {
          if($reader === $this->_server_socket) {
            $client = stream_socket_accept($this->_server_socket);
            $this->_clients[] = $client;
          } else {
            $length_packed = stream_socket_recvfrom($reader, 4, STREAM_PEEK);
            if(!strlen($length_packed)) {
              /* Client disconnected */
              $this->_clients = array_diff($this->_clients, array($reader));
            } elseif(strlen($length_packed) < 4) {
              /* Incomplete length header: leave it in the buffer for
               * next time */
              continue;
            } else {
              $unpacked = unpack('N', $length_packed);
              $length = 4 + $unpacked[1];
              $message = stream_socket_recvfrom($reader, $length, STREAM_PEEK);
              if(strlen($message) != $length) {
                /* Incomplete message: leave it for next time */
                continue;
              }
              stream_socket_recvfrom($reader, $length);
              $serialized = substr($message, 4);
              $unserialized = json_decode($serialized);
              return array($unserialized, $reader);
            }
          }
        }
      } else if ($except) {
        throw new \UnexpectedValueException("Socket error: closed");
      }
    }
  }

/**
 * Given the info returned from CompletionParser::getCompletionInfo,
 * return either a bare name or a live object suitable for passing
 * to the reflection methods for completion/documentation.
 */
  private function _getCompletionContext($info, $scope) {
    if($info->is_bare) {
      return $info->base;
    } else {
      return $this->_evalInScope('return ' . $info->base . ';', $scope);
    }
  }

/**
 * Filter completion candidates by prefix.
 */
  function _filterCompletions($candidates, $prefix, $case_fold = FALSE) {
    if(strlen($prefix) == 0) return $candidates;
    $filtered = array();
    if($case_fold) {
      $prefix_length = strlen($prefix);
      foreach($candidates as $candidate) {
        if (strpos(strtolower($candidate), strtolower($prefix)) === 0) {
          $filtered[] = $prefix . substr($candidate, $prefix_length);
        }
      }
    } else {
      foreach($candidates as $candidate) {
        if (strpos($candidate, $prefix) === 0) {
          $filtered[] = $candidate;
        }
      }
    }
    return $filtered;
  }

/**
 * Convert match data for one group from PREG_OFFSET_CAPTURE into an
 * array containing start position, end position and matched text.
 */
  private function _matchBounds($data) {
    return array($data[1], $data[1] + strlen($data[0]), $data[0]);
  }

/**
 * Return the names of all defined constants, classes, interfaces
 * and functions.
 */
  function _bareSymbols() {
    static $special = array('function', 'class', 'require', 'require_once',
                            'include', 'include_once', 'echo', 'print',
                            'unset', 'empty', 'list', 'array'); /* others? */
    $classes = get_declared_classes();
    $interfaces = get_declared_interfaces();
    $functions = array();
    foreach(get_defined_functions() as $type => $names) {
      foreach($names as $name) {
        $functions[] = $name . "(";
      }
    }
    return array_merge($special, $classes, $interfaces, $functions);
  }

/**
 * Return all properties and methods of an object.
 *
 * These are the symbols which can appear after the -> operator.
 */
  function _objectMembers($obj) {
    if(!is_object($obj)) return array();
    try {
      $refl = new \ReflectionObject($obj);
      $methods = $refl->getMethods();
      foreach ($methods as $method) {
        $return[] = $method->name . '(';
      }

      $properties = $refl->getProperties();
      foreach ($properties as $property) {
        $return[] = $property->name;
      }

      return $return;
    } catch(\ReflectionException $e) {
      return array();
    }
  }

/**
 * Return the static methods and constants of an object.
 * 
 * These are the symbols which can appear after the :: operator
 */
  function _staticMembers($obj) {
    try {
      $refl = new \ReflectionClass($obj);

      $constants = array_keys($refl->getConstants());

      $methods = array();
      foreach ($refl->getMethods(\ReflectionMethod::IS_STATIC) as $method) {
        $methods[] = $method->name . '(';
      }

      $properties = array();
      foreach($refl->getStaticProperties() as $property => $value) {
        $properties[] = '$' . $property;
      }

      return array_merge($constants, $methods, $properties);
    } catch(\ReflectionException $e) {
      return array();
    }
  }
}