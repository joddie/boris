<?php


/**
 * @file
 * Drush commands to run Boris REPL
 */

/**
 * Implements hook_drush_command().
 */
function boris_drush_command() {
  $items = array();

  $items['boris'] = array(
    'description' => dt('Run Boris REPL.'),
    'bootstrap' => DRUSH_BOOTSTRAP_MAX,
    'examples' => array(
      'drush boris' => dt('Run Boris REPL.'),
      'drush boris --listen' => dt('Run Boris REPL listening on 8015 for Emacs requests'),
    ),
    'options' => array(
      'boris' => dt('Path to Boris executable.'),
      'listen' => array(
        'description' => dt('Port to listen on.  Defaults to 8015.'),
      ),
    ),
  );
  return $items;
}

function drush_boris() {
  global $base_url;

  if (defined('DRUPAL_ROOT') && !empty($base_url)) {
    // URLs must begin with 'http://' and end with '/' for
    // drupal_override_server_variables to parse them correctly and find the
    // settings.php file
    $url = $base_url;
    if (!preg_match('|^http://|', $url)) {
      $url = "http://$url";
    }
    if (!preg_match('|/$|', $url)) {
      $url = "$url/";
    }

    $filename = tempnam('/tmp', 'drush.boris');
    $code = _drush_boris_startup_template(DRUPAL_ROOT, $url);
    $ok = file_put_contents($filename, $code);
    if ($ok === false) {
      drush_log("Failed to open temporary startup file $filename", 'error');
      exit(1);
    }
    $cmd = "boris --require=$filename";
  } else {
    drush_log('No Drupal site found.', 'warning');
    $cmd = "boris";
  }

  // FIXME: The following is lifted directly from drush_core_execute() and will
  // probably fail with remote site aliases, etc.
  $args = drush_get_original_cli_args_and_options();
  for ($x = 0; $x < sizeof($args); $x++) {
    // escape all args except for command separators.
    if (!in_array($args[$x], array('&&', '||', ';'))) {
      $args[$x] = drush_escapeshellarg($args[$x]);
    }
  }
  $cmd .= ' ' . implode(' ', $args);

  drush_shell_proc_open($cmd);
}

function _drush_boris_startup_template($dir, $url) {
  $dir = addslashes($dir);
  $url = addslashes($url);

  return "<?php\n" .  <<<EOF
  chdir('$dir');
  define('DRUPAL_ROOT', '$dir');

  require_once '$dir/includes/bootstrap.inc';

  drupal_override_server_variables(array(
      'url' => '$url',
    ));

  drupal_bootstrap(DRUPAL_BOOTSTRAP_FULL);

  // Remove Drupal's exception and error handlers to keep tracebacks
  restore_exception_handler();
  restore_error_handler();

  echo "Drupal site '$url' ($dir)\\n\\n";
EOF;
}
