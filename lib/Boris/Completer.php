<?php

/* vim: set shiftwidth=2 expandtab softtabstop=2: */

namespace Boris;

require "Completions.php";

/**
 * Performs context-sensitive completion and information lookup.
 */
class Completer {
  use Completions\AnnotateSignature;

  private $evalWorker;
  private $parser;

  public function __construct($evalWorker) {
    $this->evalWorker = $evalWorker;
    $this->parser = new CompletionParser();
  }

  /**
   * Return information for tab-completing the end of $input.
   *
   * Returns an array containing keys 'start', 'end', and
   * 'completions'.  'start' and 'end' delimit the bounds of the
   * partial symbol to be completed.  'completions' is an array of
   * possible completions.  If no completions are available, returns
   * NULL.
   */
  public function getCompletions($input, $evaluate, $scope, $annotate = false) {
    $info = $this->parser->getCompletionInfo($input);
    if($info === NULL) return NULL;

    $source = null;
    switch($info->how) {
    case CompletionParser::COMPLETE_MEMBER:
      $context = $this->getLiveContext($info->context, $evaluate, $scope);
      if ($context) {
        $source = new Completions\Members($context);
      } 
      break;

    case CompletionParser::COMPLETE_STATIC:
      $context = $this->getLiveContext($info->context, $evaluate, $scope);
      if ($context) {
        $source = new Completions\StaticMembers($context);
      } 
      break;

    case CompletionParser::COMPLETE_VARIABLE:
      $source = new Completions\Variables($scope);
      break;

    case CompletionParser::COMPLETE_INDEX:
      $context = $this->getLiveContext($info->context, $evaluate, $scope);
      $source = new Completions\ArrayIndices($context);
      break;
      
    case CompletionParser::COMPLETE_CLASS:
      $this->stripInitialSlash($info);
      $source = new Completions\ClassConstructors;
      break;

    case CompletionParser::COMPLETE_SYMBOL:
      $this->stripInitialSlash($info);
      $source = new Completions\BareNames;
      break;

    default:
      throw new \RuntimeException(sprintf(
        "Unexpected value %s returned from getCompletionInfo",
        $info->how));
    }
    if (!$source) {
      $source = new Completions\None;
    }
    
    $completions = $source->completions($info->symbol);
    $strings = array_map(function ($symbol) { return (string) $symbol; }, $completions);
    $response = array('start' => $info->start,
                      'end' => $info->end,
                      'completions' => $strings);

    if ($annotate) {
      $annotations = array();
      foreach ($completions as $candidate) {
// FIXME
        $annotations[(string) $candidate] = $candidate->annotate();
      }
      $response['annotations'] = $annotations;
    }
    return $response;
  }

  /**
   * Search all symbols and return full details.
   */
  function apropos($filter, $scope, $kinds = array()) {
    $source = new Completions\AllSymbols($scope);
    return array_map(function ($symbol) { 
      return $symbol->annotate();
    }, $source->apropos($filter));
  }

  /**
   * Return a single-line description of the arguments for the function,
   * method, or constructor at the end of $line.
   */
  public function getHint($line, $evaluate, $scope) {
    $info = $this->parser->getDocInfo($line);
    if(!$info) return NULL;
    $refl = $this->getReflectionObject($info, $evaluate, $scope);
    if(!$refl) return NULL;
    try {
      return $this->formatSignature($refl, $info->arg);
    } catch(\ReflectionException $e) {
      return NULL;
    }
  }

  /**
   * Return a multi-line string for the class, method, or function at the end of $line.
   */
  public function getDocumentation($line, $evaluate, $scope) {
    $info = $this->parser->getDocInfo($line);
    if(!$info) return NULL;
    $refl = $this->getReflectionObject($info, $evaluate, $scope);
    if(!$refl) return NULL;
    try {
      return $refl->__toString();
    } catch(\ReflectionException $e) {
      return NULL;
    }
  }

  /**
   * Return a single-line help string
   */
  public function getShortDocumentation($line, $evaluate, $scope) {
    $doc = $this->getDocumentation($line, $evaluate, $scope);
    if ($doc) {
      if (preg_match('|/[*][*]\s*\n\s*[*]\s*(.*)$|m', $doc, $matches)) {
        return $matches[1];
      }
    }
    return $this->getHint($line, $evaluate, $scope);
  }

  /**
   * Return function/method location
   */
  public function getLocation($line, $evaluate, $scope) {
    $info = $this->parser->getDocInfo($line);
    if(!$info) return NULL;
    $refl = $this->getReflectionObject($info, $evaluate, $scope);
    if(!$refl) return NULL;
    try {
      return array(
        'file' => $refl->getFileName(),
        'line' => $refl->getStartLine()
      );
    } catch(\ReflectionException $e) {
      return NULL;
    }
  }

  /**
   * Handle an annoying special case with absolutely qualified names
   */
  private function stripInitialSlash(&$info) {
    if(strlen($info->symbol) && $info->symbol[0] == '\\') {
      $info->start += 1;
      $info->symbol = substr($info->symbol, 1);
    }
  }

  /**
   * Given context information from
   * CompletionParser::getCompletionInfo or ::getDocInfo, return
   * either a bare name or a live object for passing to the
   * reflection methods.
   */
  private function getLiveContext($info, $evaluate, $scope) {
    if($info->is_bare) {
      return $info->text;
    } else {
      /* Avoid evaluating nonsense in source buffers */
      if(!$evaluate) return NULL;
      list($response, $result) = @$this->evalWorker->_forkAndEval('return ' . $info->text . ';', $scope);
      if ($response !== EvalWorker::DONE) return NULL;
      return $result;
    }
  }

  /**********************************************************************
   *
   * Private methods for hint & documentation lookup
   *
   **********************************************************************/

  /**
   * Return a ReflectionFunction or ReflectionMethod object describing
   * the function, method call, or constructor at the end of $line
   */
  private function getReflectionObject($info, $evaluate, $scope) {
    if(!$info) return NULL;
    try {
      switch($info->how) {
      case CompletionParser::FUNCTION_INFO:
        return new \ReflectionFunction($info->name);
        break;

      case CompletionParser::CLASS_INFO:
        try {
          return new \ReflectionMethod($info->name, '__construct');
        } catch (\ReflectionException $e) {
          return new \ReflectionMethod($info->name, $info->name);
        }
        break;

      case CompletionParser::METHOD_INFO:
        $context = $this->getLiveContext($info->context, $evaluate, $scope);
        return new \ReflectionMethod($context, $info->name);
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
}
