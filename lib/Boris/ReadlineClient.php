<?php

/* vim: set shiftwidth=2 expandtab softtabstop=2: */

namespace Boris;

/**
 * The Readline client is what the user spends their time entering text into.
 *
 * Input is collected and sent to {@link \Boris\EvalWorker} for processing.
 */
class ReadlineClient {
  private $_socket;
  private $_prompt;
  private $_historyFile;
  private $_clear = false;

  /**
   * Create a new ReadlineClient using $socket for communication.
   *
   * @param resource $socket
   */
  public function __construct($socket) {
    $this->_socket = $socket;
  }

  /**
   * Start the client with an prompt and readline history path.
   *
   * This method never returns.
   *
   * @param string $prompt
   * @param string $historyFile
   */
  public function start($prompt, $historyFile) {
    readline_read_history($historyFile);
    readline_completion_function(array($this, 'completion_function'));

    declare(ticks = 1);
    pcntl_signal(SIGCHLD, SIG_IGN);
    pcntl_signal(SIGINT, array($this, 'clear'), true);

    // wait for the worker to finish executing hooks
    if (fread($this->_socket, 1) != EvalWorker::READY) {
      throw new \RuntimeException('EvalWorker failed to start');
    }

    $parser = new ShallowParser();
    $buf    = '';
    $lineno = 1;

    for (;;) {
      $this->_clear = false;
      $line = readline(
        sprintf(
          '[%d] %s',
          $lineno,
          ($buf == ''
            ? $prompt
            : str_pad('*> ', strlen($prompt), ' ', STR_PAD_LEFT))
        )
      );

      if ($this->_clear) {
        $buf = '';
        continue;
      }

      if (false === $line) {
        $buf = 'exit(0);'; // ctrl-d acts like exit
      }

      if (strlen($line) > 0) {
        readline_add_history($line);
      }

      $buf .= sprintf("%s\n", $line);

      if ($statements = $parser->statements($buf)) {
        ++$lineno;

        $buf = '';
        foreach ($statements as $stmt) {
            if (false === $written = fwrite($this->_socket,
                                            EvalWorker::EVALUATE . $stmt)) {
            throw new \RuntimeException('Socket error: failed to write data');
          }

          if ($written > 0) {
            $status = fread($this->_socket, 1);
            if ($status == EvalWorker::EXITED) {
              readline_write_history($historyFile);
              echo "\n";
              exit(0);
            } elseif ($status == EvalWorker::FAILED) {
              break;
            }
          }
        }
      }
    }
  }

  /**
   * Clear the input buffer.
   */
  public function clear() {
    // FIXME: I'd love to have this send \r to readline so it puts the user on a blank line
    $this->_clear = true;

  }


  /**
   * Callback to perform readline completion.
   *
   * Sends a message over the socket to the EvalWorker requesting a
   * list of completions, in order to complete on functions &
   * variables in the REPL scope.
   */
  public function completion_function($word) {
      $rl_info = readline_info();
      if ($rl_info['library_version'] == 'EditLine wrapper') {
          print "\n\nTab completion requires PHP compiled with GNU readline, not libedit.\n\n";
          return array();
      }
      $line = substr($rl_info['line_buffer'], 0, $rl_info['point']);

      /* print("\ncompleting=$word\nprefix=$prefix\n"); */
      /* Call the EvalWorker to perform completion */
      $this->_write($this->_socket, EvalWorker::COMPLETE . $line);
      $response = $this->_read_unserialize();
      list($start, $end, $completions) = array($response->start, $response->end,
                                               $response->completions);

      $rl_start = $rl_info['point'] - strlen($word);
      $rl_end = $rl_info['point'];

      /* print("\nrl bounds=$rl_start .. $rl_end, word=$word\n"); */
      /* print("new bounds=$start .. $end\n"); */
      /* print_r($completions); */
      if($start < $rl_start) {
          foreach($completions as &$c) {
              $c = substr($c, $rl_start - $start);
          }
      } elseif($start > $rl_start) {
          foreach($completions as &$c) {
              $c = substr($line, $rl_start, $start - $rl_start) . $c;
          }
      }
      /* print_r($completions); */
      return $completions;
  }

  /* TODO: refactor me */
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
      return $written;
  }

  private function _read($socket, $bytes) {
      for($read = ''; strlen($read) < $bytes; $read .= $fread) {
          $fread = fread($socket, $bytes - strlen($read));
          if(0 === $fread) {
              throw new \RuntimeException('Socket closed during read.');
          }
      }
      return $read;
  }

  private function _read_unserialize() {
      /* Get response: expected to be one-byte opcode,
       * EvalWorker::RESPONSE, four bytes giving length of message,
       * and serialized data */
      $status = $this->_read($this->_socket, 1);
      if ($status !== EvalWorker::RESPONSE) {
          throw new \RuntimeException(sprintf('Bad response: 0x%x',
                                              ord($status)));
      }
      $length_packed = $this->_read($this->_socket, 4);
      $length_unpacked = unpack('N', $length_packed);
      $length = $length_unpacked[1];
      $serialized = $this->_read($this->_socket, $length);
      return json_decode($serialized);
  }
}