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

  /* Request opcodes sent by the ReadlineClient to EvalWorker */
  const EVALUATE  = "\0";
  const COMPLETE = "\1";

  private $_socket;
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
  public function __construct($socket) {
    $this->_socket    = $socket;
    $this->_inspector = new DumpInspector();
    stream_set_blocking($socket, 0);
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
    extract($__scope);

    $this->_write($this->_socket, self::READY);

    /* Note the naming of the local variables due to shared scope with the user here */
    for (;;) {
      declare(ticks = 1);
      // don't exit on ctrl-c
      pcntl_signal(SIGINT, SIG_IGN, true);

      $this->_cancelled = false;

      $__input = $this->_read($this->_socket);

      if ($__input === null) {
        continue;
      }

      /* Dispatch on opcode in first byte */
      switch ($__input[0]) {
      case self::EVALUATE:
        /* Expression evaluation */
        $__input = substr($__input, 1);

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

          $__result = eval($__input);

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
        break;

      case self::COMPLETE:
        $__input = substr($__input, 1);
        if(preg_match("/(->|::[$]?)([[:alpha:]_]*[[:alnum:]_]*)$/", $__input, $match_data,
                      PREG_OFFSET_CAPTURE)) {
          /* Complete one of the following, depending on the
           * dereferencing operator:
           *
           * an object property or method '->xyz_...',
           * an static method or constant '::xyz...',
           * or a static property '::$xyz...'
           */
          $__operator = $match_data[1][0];
          $__symbol = $match_data[2][0];
          list($__completion_base, $__is_bare) = $this->_getCompletionBase($__input, $match_data[0][1]);
          if($__is_bare)
            $__result = $__completion_base;
          else
            $__result = eval("return $__completion_base;");
          switch($__operator) {
          case '->':
            $__completions = $this->_objectMembers($__result);
            break;
          case '::':
            $__completions = $this->_staticMembers($__result);
            break;
          case '::$':
            $__completions = $this->_staticProperties($__result);
            break;
          default:
            /* Should never get here */
            throw new \RuntimeException(sprintf("Unknown operator '%s'", $__operator));
          }
          $__completions = $this->_filterCompletions($__completions, $__symbol);
        } elseif(preg_match('/[$]([[:alpha:]_]*[[:alnum:]_]*)$/',
                            $__input, $__match_data)) {
          /* Complete a variable name */
          $__completions = $this->_filterCompletions(array_keys(get_defined_vars()),
                                                   $match_data[1]);
        } elseif(preg_match("/\\[(['\"]?)([^\]]*)$/", $__input, $match_data,
                            PREG_OFFSET_CAPTURE)) {
          /* Complete array (hash) index */
          $__quote = $match_data[1][0];
          $__symbol = $match_data[2][0];
          list($__completion_base, $__is_bare) = $this->_getCompletionBase($__input, $match_data[0][1]);
          if($__is_bare)
            $__result = NULL;
          else
            $__result = eval("return $__completion_base;");
          $__completions = $this->_filterCompletions(array_keys($__result),
                                                   $__symbol);
          if(!$__quote) {
            foreach($__completions as &$str) {
              $str = sprintf("'%s']", str_replace("'", "\\'", $str));
            }
          }
        } elseif(preg_match("/[[:alpha:]_][[:alnum:]_]*$/", $__input, $match_data)) {
          /* Complete function, class or defined constant */
          $__completions = $this->_filterCompletions($this->_bareSymbols(), $match_data[0]);
        } else {
          $__completions = array();
        }
        $__serialized = json_encode($__completions);
        $__response = self::RESPONSE
          . pack('N', strlen($__serialized))
          . $__serialized;
        unset($__serialized, $__completions);
        break;

      default:
        throw new \RuntimeException(sprintf("Bad request code 0x%x", $__input[0]));
        $__response = self::DONE;
      }

      $this->_write($this->_socket, $__response);
      
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
    $read = array($socket);
    $write = null;
    $except = array($socket);

    if (stream_select($read, $write, $except, 10) > 0) {
      if ($read) {
        return stream_get_contents($read[0]);
      } else if ($except) {
        throw new \UnexpectedValueException("Socket error: closed");
      }
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
   * Other completion code (main loop) determines the incomplete
   * portion at the end of the line ("->x_" in the example above).
   * This function works by scanning backward from the beginning of
   * that fragment, accepting anything that looks like either a
   * constant array index or a property lookup.
   */
  function _getCompletionBase ($line, $end) {
    static $ident = "[a-zA-Z_][a-zA-Z0-9_]*";
    static $number = "\\d+";
    static $string1 = '"(?:[^"\\\\]|\\\\.)*"';
    static $string2 = "'(?:[^'\\\\]|\\\\.)*'";
    static $constant = NULL;
    if(!$constant) $constant = "(?:{$number}|{$string1}|{$string2})";

    $start = $end;
    while(TRUE) {
      $substr = substr($line, 0, $start);
      if(preg_match('/(->[$]?|::[$])' . $ident . '$/', $substr, $match)
         || preg_match('/\[' . $constant . '\]$/', $substr, $match)) {
        /* Matched either:
         * - an array index: ['str'], ["str"] or [123]
         * - property access: ->member or ->$member_name
         * - or a static property access ::$member */
        $start -= strlen($match[0]);
      } elseif(preg_match('/[$]' . $ident . '$/', $substr, $match)) {
        /* Matched a variable $var, which should be the beginning of
         * the expression. */
        $start -= strlen($match[0]);
        return array(substr($line, $start, $end - $start), FALSE);
      } elseif(preg_match("/\\\\?{$ident}(?:\\\\{$ident})*$/", $substr, $match)) {
        $is_bare = $start == $end;
        $start -= strlen($match[0]);
        return array(substr($line, $start, $end - $start), $is_bare);
      } else {
        /* Something else: assume we don't want to evaluate it for
         * tab-completion.
         */
        return FALSE;
      }
    }
  }

  /**
   * Filter possible completions by prefix
   */
  function _filterCompletions($candidates, $prefix) {
    $completions = array();
    foreach($candidates as $candidate) {
      if(strpos($candidate, $prefix) == 0) {
        $completions[] = $candidate;
      }
    }
    return $completions;
  }

  /**
   * Return the names of all defined constants, classes, interfaces
   * and functions.
   */
  function _bareSymbols() {
    $constants = array_keys(get_defined_constants());
    $classes = get_declared_classes();
    $interfaces = get_declared_interfaces();
    $functions = array();
    foreach(get_defined_functions() as $type => $names) {
      foreach($names as $name) {
        $functions[] = $name . "(";
      }
    }
    return array_merge($constants, $classes, $interfaces, $functions);
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
      $methods = $refl->getMethods(/* \ReflectionMethod::IS_PUBLIC */);
      foreach ($methods as $method) {
        $return[] = $method->name . '(';
      }

      $properties = $refl->getProperties(/* \ReflectionProperty::IS_PUBLIC */);
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
      return array_merge($methods, $constants);
    } catch(\ReflectionException $e) {
      return array();
    }
  }

  /**
   * Return static variables for an object
   *
   * These appear after the :: operator, preceded by a $ sigil.
   */
  function _staticProperties($obj) {
    try {
      $refl = new \ReflectionClass($obj);
      return array_keys($refl->getStaticProperties());
    } catch(\ReflectionException $e) {
      return array();
    }
  }
}
