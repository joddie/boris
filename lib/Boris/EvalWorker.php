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
  private $_buffers = array();
  private $_clients = array();
  private $_exports = array();
  private $_startHooks = array();
  private $_failureHooks = array();
  private $_ppid;
  private $_pid;
  private $_cancelled;
  private $_inspector;
  private $_exceptionHandler;
  private $_completer;

  /**
   * Create a new worker using the given socket for communication.
   *
   * @param resource $socket
   */
  public function __construct($socket, $port) {
    $this->_socket    = $socket;
    $this->_inspector = new DumpInspector();
    stream_set_blocking($socket, 0);

    $this->_buffers[(int) $this->_socket] = '';

    if(!empty($port)) {
      $this->_server_socket = stream_socket_server("tcp://127.0.0.1:$port", $errno, $errstr);
      if(!$this->_server_socket) {
        print "Error starting server on port $port: '$errstr' ($errno)\n";
      } else {
        print "Listening on port $port.\n";
      }
    }

    $this->_completer = new Completer($this);
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

      if ($input->operation == 'evaluate') {
        // Evaluation returns a one-byte success-or-failure code
        $__response = $this->_evalAndPrint($input->statement, $__scope);
      } else {
        // Documentation lookup and cross-reference commands return JSON data
        switch ($input->operation) {
        case 'complete':
          $return = $this->_completer->getCompletions($input->line, $input->evaluate, $__scope);
          break;

        case 'annotate':
          $return = $this->_completer->getCompletions($input->line, $input->evaluate, $__scope, true);
          break;

        case 'completesymbol':
          $return = $this->_completer->completeSymbol($input->prefix, $input->kind, $__scope, $input->annotate);
          break;
          
        case 'hint':
          $return = $this->_completer->getHint($input->line, $input->evaluate, $__scope);
          break;

        case 'documentation':
          $return = $this->_completer->getDocumentation($input->line, $input->evaluate, $__scope);
          break;

        case 'shortdoc':
          $return = $this->_completer->getShortDocumentation($input->line, $input->evaluate, $__scope);
          break;

        case 'location':
          $return = $this->_completer->getLocation($input->line, $input->evaluate, $__scope);
          break;

        case 'apropos':
          $return = $this->_completer->apropos($input->regexp, $__scope);
          break;
        
        case 'whouses':
          $return = $this->_completer->whoUses($input->trait);
          break;

        case 'whoimplements':
          $return = $this->_completer->whoImplements($input->interface);
          break;

        case 'whoextends':
          $return = $this->_completer->whoExtends($input->class);
          break;

        default:
          throw new \RuntimeException(sprintf("Bad operation '%s'", $input->operation));
        }
        $__response = $this->_packResponse($return);
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

  private function _evalAndPrint($input, &$scope) {
    $input = $this->_transform($input);
    list($response, $result) = $this->_forkAndEval($input, $scope);
    
    if (preg_match('/\s*return\b/i', $input)) {
      fwrite(STDOUT, sprintf(" → %s\n", $this->_inspector->inspect($result)));
    }

    return $response;
  }

  public function _forkAndEval($__input, &$__scope) {
    $__result = NULL;
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
      $this->_expungeOldWorker();
    }
    return array($__response, $__result);
  }

  public function _evalInScope($__input, &$__scope) {
    extract($__scope);
    $__result = eval($__input);
    $__scope = array_diff_key(get_defined_vars(), array(
      '__input' => 1,
      '__scope' => 1,
      '__result' => 1));
    return $__result;
  }

  private function _expungeOldWorker() {
    posix_kill($this->_ppid, SIGTERM);
    pcntl_signal_dispatch();
  }

  private function _write($socket, $data) {
    $total = strlen($data);
    for ($written = 0; $written < $total; $written += $fwrite) {
      $fwrite = @fwrite($socket, substr($data, $written));
      if ($fwrite === false) {
        throw new \RuntimeException(
          sprintf('Socket error: wrote only %d of %d bytes.',
                  $written, $total));
      }
    }
  }

  private function _packResponse($response) {
    $serialized = json_encode($response);
    return self::RESPONSE . pack('N', strlen($serialized)) . $serialized;
  }

  private function _read($socket)
  {
    /* Process any buffered requests before blocking on more data */
    $pending = $this->_pendingRequest();
    if ($pending) return $pending;

    $sockets = array_values($this->_clients);
    $sockets[] = $socket;
    if($this->_server_socket)
      $sockets[] = $this->_server_socket;

    $read = $except = $sockets;

    if ($this->_select($read, $except) > 0) {
      if ($read) {
        foreach ($read as $reader) {
          if($reader === $this->_server_socket) {
            /* Make a new client connection */
            $client = stream_socket_accept($this->_server_socket);
            $id = (int) $client;
            $this->_clients[$id] = $client;
            $this->_buffers[$id] = '';
          } else {
            $id = (int) $reader;
            $data = stream_socket_recvfrom($reader, 4096);
            if (!strlen($data)) {
              /* Client disconnected: remove its buffer and socket */
              unset($this->_clients[$id]);
              unset($this->_buffers[$id]);
            } else {
              /* Received data: append it to the buffer to be
               * processed on next call to _read */
              $this->_buffers[$id] .= $data;
            }
          }
        }
      } else if ($except) {
        throw new \UnexpectedValueException("Socket error: closed");
      }
    }
  }

  private function _pendingRequest() {
    foreach ($this->_buffers as $id => &$buf) {
      if (strlen($buf) < 4) {
        /* Empty buffer, or incomplete length header */
        continue;
      }

      $unpacked = unpack('N', $buf);
      $json_length = $unpacked[1];
      $msg_length = 4 + $unpacked[1];

      if (strlen($buf) < $msg_length) {
        /* Incomplete message  */
        continue;
      }

      $serialized = substr($buf, 4, $json_length);
      $unserialized = json_decode($serialized);
      $buf = substr($buf, $msg_length);
      return array($unserialized, $id == (int) $this->_socket ? $this->_socket : $this->_clients[$id]);
    }
    return null;
  }

  private function _select(&$read, &$except) {
    $write = null;
    set_error_handler(function(){return true;}, E_WARNING);
    $result = stream_select($read, $write, $except, 10);
    restore_error_handler();
    return $result;
  }

  private function _transform($input) {
    if ($input === null) {
      return null;
    }

    $transforms = array(
      'exit' => 'exit(0)'
    );

    foreach ($transforms as $from => $to) {
      $input = preg_replace('/^\s*' . preg_quote($from, '/') . '\s*;?\s*$/', $to . ';', $input);
    }

    return $input;
  }
}
