<?php

/* vim: set shiftwidth=2 expandtab softtabstop=2: */

namespace Boris;

/**
 * Processes available command line flags.
 */
class CLIOptionsHandler {
  /**
   * Accept the REPL object and perform any setup necessary from the CLI flags.
   *
   * @param Boris $boris
   */
  public function handle($boris) {
    $args = getopt('hvr:l::', array('help', 'version', 'require:', 'listen::'));

    foreach ($args as $option => $value) {
      switch ($option) {
        /*
         * Sets files to load at startup, may be used multiple times,
         * i.e: boris -r test.php,foo/bar.php -r ba/foo.php --require hey.php
         */
        case 'r':
        case 'require':
          $this->_handleRequire($boris, $value);
        break;

        /*
         * Show Usage info
         */
        case 'h':
        case 'help':
          $this->_handleUsageInfo();
        break;

        /*
         * Show version
         */
        case 'v':
        case 'version':
          $this->_handleVersion();
        break;

        /*
         * Setup listening socket
         */
        case 'l':
        case 'listen':
          if($value === FALSE) $value = 8015;
          if(!is_numeric($value)) {
            fprintf(STDERR, "-l or --listen requires a numeric port number (default 8015)");
            die(1);
          }
          $boris->listenOn($value);
        break;
      }
    }
  }

  // -- Private Methods

  private function _handleRequire($boris, $paths) {
    $require = array_reduce(
      (array) $paths,
      function($acc, $v) { return array_merge($acc, explode(',', $v)); },
      array()
    );

    $boris->onStart(function($worker, $scope) use($require) {
      foreach($require as $path) {
        require $path;
      }

      $worker->setLocal(get_defined_vars());
    });
  }

  private function _handleUsageInfo() {
    echo <<<USAGE
Usage: boris [options]
boris is a tiny REPL for PHP

Options:
  -h, --help     show this help message and exit
  -r, --require  a comma-separated list of files to require on startup
  -v, --version  show Boris version
  -l, --listen [port]
                 listen for connections on the given port (default 8015)

USAGE;
    exit(0);
  }

  private function _handleVersion() {
    printf("Boris %s\n", Boris::VERSION);
    exit(0);
  }
}
