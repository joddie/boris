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
        $__response = $this->_doComplete($input->line, $__scope);
        break;

      case 'hint':
        $__response = $this->_doHint($input->what, $__scope);
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
    if(preg_match("/(->|::)[[:space:]]*([$]?[[:alpha:]_]*[[:alnum:]_]*)$/",
                  $input, $matches, PREG_OFFSET_CAPTURE)) {
      /* Complete one of the following, depending on the
       * dereferencing operator:
       *
       * an object property or method '->xyz_...',
       * an static method or constant '::xyz...',
       * or a static property '::$xyz...'
       */
      $operator = $matches[1][0];
      list($start, $end, $symbol) = $this->_matchBounds($matches[2]);
      $obj = $this->_getCompletionObject($input, $matches[0][1], $scope);
      if($operator == '->')
        $candidates = $this->_objectMembers($obj);
      else
        $candidates = $this->_staticMembers($obj);
      $completions = $this->_filterCompletions($candidates, $symbol);
    } elseif(preg_match('/[$]([[:alpha:]_]*[[:alnum:]_]*)$/',
                        $input, $matches, PREG_OFFSET_CAPTURE)) {
      /* Complete variable name */
      list($start, $end, $symbol) = $this->_matchBounds($matches[1]);
      $completions = $this->_filterCompletions(array_keys($scope), $symbol);
    } elseif(preg_match("/\\[[[:space:]]*(['\"]?)([^\]]*)$/",
                        $input, $matches, PREG_OFFSET_CAPTURE)) {
      /* Complete array (hash) index */
      $quote = $matches[1][0];
      list($start, $end, $symbol) = $this->_matchBounds($matches[2]);
      $obj = $this->_getCompletionObject($input, $matches[0][1], $scope);
      if(!is_array($obj)) {
        $completions = array();
      } else {
        $completions = $this->_filterCompletions(array_keys($obj), $symbol);
        if(!$quote) {
          foreach($completions as &$str) {
            $str = sprintf("'%s']", str_replace("'", "\\'", $str));
          }
        }
      }
    } elseif(preg_match("/new[[:space:]]+\\\\?((?:[[:alpha:]_]*[[:alnum:]_]*)(?:\\\\([[:alpha:]_]+[[:alnum:]_]*))*\\\\?)$/",
                        $input, $matches, PREG_OFFSET_CAPTURE)) {
      /* Complete "new ..." with a class name */
      list($start, $end, $symbol) = $this->_matchBounds($matches[1]);
      $completions = $this->_filterCompletions(get_declared_classes(), $symbol);
      foreach($completions as &$completion) $completion .= '(';
    } elseif(preg_match("/\\\\?((?:[[:alpha:]_]+[[:alnum:]_]*)(?:\\\\([[:alpha:]_]+[[:alnum:]_]*))*\\\\?)$/",
                        $input, $matches, PREG_OFFSET_CAPTURE)) {
      /* Complete function, class, interface, or constant */
      list($start, $end, $symbol) = $this->_matchBounds($matches[1]);
      $completions = $this->_filterCompletions($this->_bareSymbols(), $symbol);
    } else {
      /* Something else we don't know how to complete. */
      $start = $end = NULL;
      $completions = array();
    }
    return $this->_packResponse(array('start' => $start,
                                      'end' => $end,
                                      'completions' => $completions));
  }

  private function _packResponse($response) {
    $serialized = json_encode($response);
    return self::RESPONSE . pack('N', strlen($serialized)) . $serialized;
  }

  private function _doHint($what, $scope) {
    try {
      $refl = new \ReflectionFunction($what);
      $params = $refl->getParameters();
      $required = array_splice($params, 0, $refl->getNumberOfRequiredParameters());
      $arg_string = $this->_describeParams($required);
      if(count($params)) {
        $arg_string .= sprintf('[, %s ]', $this->_describeParams($params));
      }
      $response = array('hint' =>
                        sprintf('%s%s ( %s )',
                                $refl->returnsReference() ? '&' : '',
                                $refl->name,
                                $arg_string));
    } catch(\ReflectionException $e) {
      $response = array('hint' => NULL);
    }
    return $this->_packResponse($response);
  }

  private function _describeParams($params) {
    return implode(', ', array_map(function($param) {
      $base = sprintf('%s$%s',
                      $param->isPassedByReference() ? '&' : '',
                      $param->name);
      if($param->isDefaultValueAvailable())
        return $base . " = " . $param->getDefaultValue();
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
            $length_packed = fread($reader, 4);
            if(!strlen($length_packed)) {
              $this->_clients = array_diff($this->_clients, array($reader));
            } else {
              $unpacked = unpack('N', $length_packed);
              $length = $unpacked[1];
              /* FIXME: This really has to be in a loop and use
               * temporary buffers, because the whole input may not
               * arrive at once. */
              $serialized = fread($reader, $length); /* stream_get_contents($reader); */
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
   * Given an incomplete line of code, return either a live object or
   * a name to pass to reflection methods for member completion.
   */
  private function _getCompletionObject($line, $end, $scope) {
    list($expr, $is_bare) = $this->_getCompletionBase($line, $end);
    if($is_bare) {
      return $expr;
    } else {
      return $this->_evalInScope("return $expr;", $scope);
    }
  }

  /**
   * Get the portion of code which must be evaluated to perform
   * tab-completion.
   *
   * Example: if the cursor is at the end of the following line:
   *   printf("%d\n", $arr['index'][123]->member->x_
   * then the portion of the line which needs evaluation is
   *   $arr['index'][123]->member
   *   
   * Other completion code in _doComplete matches the incomplete
   * portion at the end of the line ("->x_" in the example above).
   * This function works by scanning backward from the beginning of
   * that fragment, accepting anything that looks like either a
   * constant array index or a property lookup.
   */
  function _getCompletionBase ($line, $end) {
    /* Construct static regexps on first call */
    static $rxMemberOrIndex, $rxVariable, $rxQualifiedName;
    if(!$rxMemberOrIndex) {
      $identifier = "[a-zA-Z_][a-zA-Z0-9_]*";
      $number = "\\d+";
      $singleString = '"(?:[^"\\\\]|\\\\.)*"';
      $doubleString = "'(?:[^'\\\\]|\\\\.)*'";

      /* A variable with $ sigil */
      $variable = "[$]{$identifier}";

      /* A possibly-namespace-qualified bare name */
      $qualifiedName = "\\\\?{$identifier}(?:\\\\{$identifier})*";

      /* A class or interface constant */
      $classConstant = "{$qualifiedName}::{$identifier}";

      /* A constant: string literal, number, bare name, or class constant */
      $constant = "(?:{$number}|{$identifier}|{$classConstant}|{$singleString}|{$doubleString})";

      /* A constant array index: [ ... ] */
      $index = "\\[[[:space:]]*{$constant}[[:space:]]*\\]";

      /* A property access: ->xyz, ->$xyz, ::xyz, or ::$xyz */
      $member = "(->|::)[[:space:]]*[$]?{$identifier}";

      /* Either a property access or an index */
      $memberOrIndex = "(?:(?:{$member})|(?:{$index}))";
      
      /* Static values including string-end assertion and PCRE delimiters */
      $rxMemberOrIndex = "/{$memberOrIndex}$/";
      $rxVariable = "/{$variable}$/";
      $rxQualifiedName = "/{$qualifiedName}$/";
    }

    $start = $end = strlen(rtrim(substr($line, 0, $end)));
    while(TRUE) {
      /* Ignore any whitespace at string end */
      $substr = rtrim(substr($line, 0, $start));
      $start = strlen($substr);

      if(preg_match($rxMemberOrIndex, $substr, $match)) {
        /* Matched a dereference or an array index.  Walk back and try
         * to match more. */ 
        $start -= strlen($match[0]);
      } elseif(preg_match($rxVariable, $substr, $match)) {
        /* Matched a variable $var at the beginning of the expression. */
        $start -= strlen($match[0]);
        return array(substr($line, $start, $end - $start), FALSE);
      } elseif(preg_match($rxQualifiedName, $substr, $match)) {
        /* Matched a bare name at the beginning of the expression. */

        /* If this bare name is the entire 'expression', it's a class
         * or interface name which should be passed as a string to the
         * reflection methods, not evaluated. */
        $is_bare = $start == $end;
        $start -= strlen($match[0]);
        return array(substr($line, $start, $end - $start), $is_bare);
      } else {
        /* Found something else.  Assume we don't want to evaluate it
         * for tab-completion.
         */
        return FALSE;
      }
    }
  }

  /**
   * Filter possible completions by prefix
   */
  function _filterCompletions($candidates, $prefix) {
    if(strlen($prefix) == 0) return $candidates;
    $completions = array();
    foreach($candidates as $candidate) {
      if(strpos($candidate, $prefix) === 0) {
        $completions[] = $candidate;
      } 
    }
    return $completions;
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
                            'unset', 'empty'); /* others? */
    $constants = array_keys(get_defined_constants());
    $classes = get_declared_classes();
    $interfaces = get_declared_interfaces();
    $functions = array();
    foreach(get_defined_functions() as $type => $names) {
      foreach($names as $name) {
        $functions[] = $name . "(";
      }
    }
    return array_merge($special, $constants, $classes, $interfaces, $functions);
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
