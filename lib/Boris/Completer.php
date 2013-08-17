<?php

/* vim: set shiftwidth=2 expandtab softtabstop=2: */

namespace Boris;

/**
 *
 */
class Completer {

  private $evalWorker;
  private $parser;

  static $PHP_KEYWORDS = array(
    /* taken from http://php.net/manual/en/reserved.keywords.php */
    '__halt_compiler',
    'abstract', 'and', 'array', 'as', 'break', 'callable', 'case', 'catch',
    'class', 'clone', 'const', 'continue', 'declare', 'default', 'die',
    'do', 'echo', 'else', 'elseif', 'empty', 'enddeclare', 'endfor', 'endforeach',
    'endif', 'endswitch', 'endwhile', 'eval', 'exit', 'extends',
    'final', 'for', 'foreach', 'function', 'global', 'goto', 'if', 'implements',
    'include', 'include_once', 'instanceof', 'insteadof', 'interface', 'isset',
    'list', 'namespace', 'new', 'or', 'print', 'private', 'protected', 'public',
    'require', 'require_once', 'return', 'static', 'switch', 'throw', 'trait',
    'try', 'unset', 'use', 'var', 'while', 'xor',

    /* Not sure if these are really necessary.. */
    '__CLASS__', '__DIR__', '__FILE__', '__FUNCTION__', '__LINE__', '__METHOD__',
    '__NAMESPACE__', '__TRAIT__');

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
  public function getCompletions($input, $scope) {
    $info = $this->parser->getCompletionInfo($input);
    if($info === NULL) return NULL;

    switch($info->how) {
    case CompletionParser::COMPLETE_MEMBER:
    case CompletionParser::COMPLETE_STATIC:
      $context = $this->getCompletionContext($info, $scope);
      if($info->how == CompletionParser::COMPLETE_MEMBER)
        list($properties, $methods) = $this->objectMembers($context);
      else
        list($properties, $methods) = $this->staticMembers($context);
      /* Constants and properties are case sensitive. Functions and
       * methods are not. */
      $properties = $this->filterCompletions($properties, $info->symbol);
      $methods = $this->filterCompletions($methods, $info->symbol, TRUE);
      $completions = array_merge($properties, $methods);
      break;

    case CompletionParser::COMPLETE_VARIABLE:
      $candidates = array_map(function($name) { return '$' . $name; },
                              array_keys($scope));
      $completions = $this->filterCompletions($candidates, $info->symbol);
      break;

    case CompletionParser::COMPLETE_INDEX:
      $context = $this->getCompletionContext($info, $scope);
      if(is_array($context)) {
        $completions = $this->filterCompletions(array_keys($context),
                                                $info->symbol);
      } else {
        $completions = array();
      }
      break;

    case CompletionParser::COMPLETE_CLASS:
      $this->stripInitialSlash($info);
      $completions = $this->filterCompletions(get_declared_classes(),
                                               $info->symbol, TRUE);
      $completions = array_map(function($name) { return $name . '('; },
                               $completions);
      break;

    case CompletionParser::COMPLETE_SYMBOL:
      $this->stripInitialSlash($info);
      /* Constants are case sensitive, but other names are not. */
      $constants = $this->filterCompletions(array_keys(get_defined_constants()),
                                            $info->symbol);
      $symbols = $this->filterCompletions($this->bareSymbols(),
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

  /**
   * Return a single-line description of the arguments for the function,
   * method, or constructor at the end of $line.
   */
  public function getHint($line, $scope) {
    $refl = $this->getReflectionObject($line, $scope);
    if(!$refl) return NULL;
    try {
      return $this->formatFunction($refl);
    } catch(\ReflectionException $e) {
      return NULL;
    }
  }

  /**
   * Return a multi-line string for the class, method, or function at the end of $line.
   */
  public function getDocumentation($line, $scope) {
    $refl = $this->getReflectionObject($line, $scope);
    if(!$refl) return NULL;
    try {
      return $refl->__toString();
    } catch(\ReflectionException $e) {
      return NULL;
    }
  }

  /**********************************************************************
   *
   * Private methods for performing completion
   *
   **********************************************************************/

  /* Handle an annoying special case with absolutely qualified names */
  private function stripInitialSlash(&$info) {
    if(strlen($info->symbol) && $info->symbol[0] == '\\') {
      $info->start += 1;
      $info->symbol = substr($info->symbol, 1);
    }
  }

  /**
   * Given the info returned from CompletionParser::getCompletionInfo,
   * return either a bare name or a live object suitable for passing
   * to the reflection methods for completion/documentation.
   */
  private function getCompletionContext($info, $scope) {
    if($info->is_bare) {
      return $info->base;
    } else {
      /* FIXME: This should be evaluated safely by forking, as with normal evaluation */
      return $this->evalWorker->_evalInScope('return ' . $info->base . ';', $scope);
    }
  }

  /**
   * Filter completion candidates by prefix.
   */
  private function filterCompletions($candidates, $prefix, $case_fold = FALSE) {
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
   * Return the names of all defined classes, interfaces, functions, and special constructs.
   */
  private function bareSymbols() {
    $classes = get_declared_classes();
    $interfaces = get_declared_interfaces();
    $functions = array();
    foreach(get_defined_functions() as $type => $names) {
      foreach($names as $name) {
        $functions[] = $name . "(";
      }
    }
    return array_merge(self::$PHP_KEYWORDS, $classes, $interfaces, $functions);
  }

  /**
   * Return the names of properties and methods of an object.
   * These are the symbols which can appear after the -> operator.
   *
   * Properties are case sensitive, but methods are not.
   */
  private function objectMembers($obj) {
    if(!is_object($obj)) return array(array(), array());
    try {
      $refl = new \ReflectionObject($obj);
      $methods = array_map(function ($method) { return $method->name . '('; },
                           $refl->getMethods(\ReflectionMethod::IS_PUBLIC));
      $properties = array_map(function ($prop) { return $prop->name; },
                              $refl->getProperties(\ReflectionProperty::IS_PUBLIC));

      return array($properties, $methods);
    } catch(\ReflectionException $e) {
      return array(array(), array());
    }
  }

/**
 * Return the static methods, properties and constants of an object or class.
 * These are the symbols which can appear after the :: operator.
 *
 * Properties and constants are case sensitive, but methods are not.
 */
  private function staticMembers($obj) {
    try {
      $refl = new \ReflectionClass($obj);

      $methods = array_map(function ($method) { return $method->name . '('; },
                           $refl->getMethods(\ReflectionMethod::IS_STATIC));
      $properties = array_merge(array_keys($refl->getConstants()),
                                array_map(function ($prop_name) { return '$' . $prop_name; },
                                          array_keys($refl->getStaticProperties(
                                            \ReflectionProperty::IS_PUBLIC))));
      return array($properties, $methods);

    } catch(\ReflectionException $e) {
      return array(array(), array());
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
  private function getReflectionObject($line, $scope) {
    $info = $this->parser->getDocInfo($line);
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
        /* FIXME: This should be evaluated safely by forking */
        if($bare) $obj = $base;
        else $obj = $this->evalWorker->_evalInScope("return $base;", $scope);
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

  /**
   * Format information about function / method arguments, for getHint()
   */
  private function formatFunction($refl) {
    $params = $refl->getParameters();
    $required = array_splice($params, 0, $refl->getNumberOfRequiredParameters());
    $arg_string = $this->formatParams($required);
    if(count($params)) {
      $arg_string .= sprintf(' [, %s ]', $this->formatParams($params));
    }

    return sprintf('%s%s ( %s )',
                   $refl->returnsReference() ? '&' : '',
                   ($refl->name == '__construct') ? $refl->getDeclaringClass()->name : $refl->name,
                   $arg_string);
  }

  private function formatParams($params) {
    return implode(', ', array_map(function($param) {
      $class = $param->getClass();
      $base = sprintf('%s%s$%s',
                      $class ? "$class " : '',
                      $param->isPassedByReference() ? '&' : '',
                      $param->name);
      if($param->isDefaultValueAvailable())
        return $base . " = " . var_export($param->getDefaultValue(), TRUE);
      else
        return $base;
    }, $params));
  }

}
