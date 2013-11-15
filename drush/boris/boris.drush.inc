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
  $which_boris = drush_get_option('boris');
  if (!$which_boris) {
    $which_boris = exec('which boris');
  }
  $which_boris = is_link($which_boris) ? readlink($which_boris) : $which_boris;
  if (!($which_boris && is_file($which_boris) && is_readable($which_boris))) {
    return drush_set_error('NO_SUCH_FILE',
      dt('Boris executable "@boris" not found or not readable.', array(
          '@boris' => $which_boris
        )));
  }

  $autoload = dirname($which_boris) . '/../lib/autoload.php';
  require_once $autoload;

  $boris = new \Boris\Boris('drupal> ');
  $boris->setInspector(new \Boris\ExportInspector());
  if ($listen = drush_get_option('listen')) {
    if ($listen === true) {
      $listen = 8015;
    }
    drush_log(dt('Listening on port @listen', array('@listen' => $listen)), 'notice');
    $boris->listenOn($listen);
  }

  drush_log(dt('Starting Boris...'));
  $boris->start();
}
