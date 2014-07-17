<?php

namespace Boris\Completions;

/**
 * A completion source defines how to look up a kind of PHP symbol.
 *
 * Concrete implementations of this interface exist for globals,
 * variables, functions, classes, object members, etc.
 *
 * Completion sources return Symbols, which can be converted to
 * strings or queried for further information using annotate()
 */
interface Source {
  public function symbols();
  public function completions($prefix);
  public function apropos($query);
}

interface Symbol {
  public function kind();
  public function annotate();
}

/*************************************************************
 *
 * Merged completion sources for particular contexts
 *
 *************************************************************/

/**
 * Merge several sources together
 */
class MergeSources implements Source {
  private $sources;
  
  public function __construct($sources) {
    foreach ($sources as $source) {
      assert ($source instanceof Source);
    }
    $this->sources = $sources;
  }

  public function symbols() {
    return $this->merge(
        function ($source) {
          return $source->symbols();
        }
    );
  }
   
  public function completions($prefix) {
    return $this->merge(
        function ($source) use ($prefix) {
          return $source->completions($prefix);
        }
    );
  }

  public function apropos($query) {
    return $this->merge(
      function ($source) use ($query) {
        return $source->apropos($query);
      }
    );
  }

  private function merge($function) {
    return call_user_func_array(
      'array_merge',
      array_map($function, $this->sources)
    );
  }
}

/**
 * Completion source for all bare symbols
 */
class BareNames extends MergeSources {
  public function __construct() {
    parent::__construct(array(
      new Keywords(),
      new Constants(),
      new Functions(),
      new ClassNames(),
      new Interfaces(),
      new Traits(),
    ));
  }
}

/**
 * Completion and info source for object members accessible after "->"
 */
class Members extends MergeSources {
  public function __construct($context) {
    parent::__construct(array(
      new Methods($context),
      new Properties($context),
    ));
  }
}

/**
 * Completion source for class/object members accessible after "::".
 */
class StaticMembers extends MergeSources {
  public function __construct($context) {
    parent::__construct(array(
      new StaticMethods($context),
      new StaticProperties($context),
      new ClassConstants($context),
    ));
  }
}

/************************************************************
 *
 * Case-sensitive and -insensitive completion 
 *
 ************************************************************/


/**
 * Apropos
 */
trait Apropos {
  function apropos($filter) {
    $candidates = $this->symbols();
    if (empty($filter)) {
      return $candidates;
    }

    if (is_string($filter)) {
      $preg = '/' . $filter . '/i';
      $predicate = function ($symbol) use ($preg) {
        return preg_match($preg, (string) $symbol);
      };
    }
    return array_filter($candidates, $predicate);
  }
}

/**
 * Case insensitive
 */
trait CaseInsensitive {
  use Apropos;
  public function completions($prefix) {
    if (strlen($prefix) == 0) {
      return $this->symbols();
    } else {
      $prefix_lower = strtolower($prefix);
      return array_filter($this->symbols(), function ($symbol) use ($prefix_lower) {
        return (strpos(strtolower($symbol), $prefix_lower) === 0);
      });
    }
  }
}

/**
 * Case sensitive
 */
trait CaseSensitive {
  use Apropos;
  public function completions($prefix) {
    if (strlen($prefix) == 0) {
      return $this->symbols();
    } else {
      return array_filter($this->symbols(), function ($symbol) use ($prefix) {
        return (strpos($symbol, $prefix) === 0);
      });
    }
  }
}

/**
 * Trait: return a specified type of Symbol for each name
 */
trait SymbolSource {
  abstract protected function names();
  abstract protected function symbol($name);
  
  public function symbols() {
    return array_map(array($this, 'symbol'), $this->names());
  }
}

/************************************************************
 * 
 * Globally-defined completions
 *
 ************************************************************/

/**
 * Dummy source with no completions
 */
class None implements Source {
  public function symbols() {
    return array();
  }

  public function completions($prefix) {
    return array();
  }

  public function apropos($query) {
    return array();
  }
}

/**
 * Variables.
 */
class Variables implements Source {
  use SymbolSource, CaseInsensitive;

  private $scope;
  public function __construct(array $scope) {
    $this->scope = $scope;
  }

  protected function names() {
    return array_merge(
      array_keys($this->scope),
      array_keys($GLOBALS)
    );
  }

  protected function symbol($name) {
    return new Variable($name);
  }
}

/**
 * Functions.
 */
class Functions implements Source {
  use SymbolSource, CaseInsensitive;

  protected function names() {
    $funcs = get_defined_functions();
    return array_merge($funcs['internal'], $funcs['user']);
  }

  protected function symbol($name) {
    return new FunctionName($name);
  }
}

/**
 * Global constants.
 */
class Constants implements Source {
  use SymbolSource, CaseSensitive;
  
  function names() {
    return array_keys(get_defined_constants());
  }

  function symbol($name) {
    return new Constant($name);
  }
}

/**
 * Built-in keywords
 * 
 * List taken from http://php.net/manual/en/reserved.keywords.php
 */
class Keywords implements Source {
  use SymbolSource, CaseInsensitive;

  static $keywords = array(
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
    '__CLASS__', '__DIR__', '__FILE__', '__FUNCTION__', '__LINE__', '__METHOD__',
    '__NAMESPACE__', '__TRAIT__'
  );
  
  protected function names() {
    return self::$keywords;
  }

  protected function symbol($name) {
    return new Keyword($name);
  }
}

/**
 * Class names used as bare symbols
 */
class ClassNames implements Source {
  use SymbolSource, CaseInsensitive;

  function names() {
    return get_declared_classes();
  }

  function symbol($name) {
    return new ClassName($name);
  }
}

/**
 * Class names used as constructors.
 */
class ClassConstructors extends ClassNames {
  function symbol($name) {
    return new ClassConstructor($name);
  }
}

/**
 * Interfaces
 */
class Interfaces implements Source {
  use SymbolSource, CaseInsensitive;
  
  protected function names() {
    return get_declared_interfaces();
  }

  protected function symbol($name) {
    return new InterfaceName($name);
  }
}

/**
 * Traits
 */
class Traits implements Source {
  use SymbolSource, CaseInsensitive;
  
  protected function names() {
    return get_declared_traits();
  }

  protected function symbol($name) {
    return new TraitName($name);
  }
}


/************************************************************
 *
 * Completion of object/class members.
 *
 ************************************************************/

/**
 * Trait: use reflection on an object or class
 */
trait ReflectObject {
  private $refl;
    
  public function __construct($context) {
    try {
      if (is_object($context)) {
        $this->refl = new \ReflectionObject($context);
      } elseif (is_string($context)) {
        $this->refl = new \ReflectionClass($context);
      }
    } catch(\ReflectionException $e) {
      $this->refl = null;
    }
    if (!$this->refl) {
      // Use an empty dummy object 
      $this->refl = new \ReflectionObject(new \stdClass);
    }
  }
}

/**
 * Trait: use reflection on object or class members
 */
trait ReflectObjectMembers {
  use ReflectObject;
  abstract protected function members();
  abstract protected function symbol($name);

  public function symbols() {
    return array_map(array($this, 'symbol'), $this->names());
  }

  protected function names() {
    return array_map(function (\Reflector $member) {
      return $member->name;
    }, $this->members());
  }
}

/**
 * Trait: look up methods
 */
trait ReflectObjectMethods {
  use ReflectObjectMembers, CaseInsensitive;
  abstract protected function modifier();
  protected function members() {
    return $this->refl->getMethods($this->modifier());
  }
}

/**
 * Trait: look up properties
 */
trait ReflectObjectProperties {
  use ReflectObjectMembers, CaseSensitive;
  abstract protected function modifier();
  protected function members() {
    return $this->refl->getProperties($this->modifier());
  }
}

/**
 * Convenience trait: look up only public members.
 */
trait PublicVisibility {
  protected function modifier() { return \ReflectionMethod::IS_PUBLIC; }
}

/**
 * Convenience trait: look up only static members.
 */
trait StaticVisibility {
  protected function modifier() { return \ReflectionMethod::IS_STATIC; }
}

/**
 * Instance methods.
 */
class Methods implements Source {
  use ReflectObjectMethods, PublicVisibility;
  protected function symbol($name) {
    return new MethodName($this->refl, $name);
  }
}

/**
 * Static methods.
 */
class StaticMethods implements Source {
  use ReflectObjectMethods, StaticVisibility;
  protected function symbol($name) {
    return new StaticMethodName($this->refl, $name);
  }
}

/**
 * Instance properties.
 */
class Properties implements Source {
  use ReflectObjectProperties, PublicVisibility {
    members as properties;
  }

  /**
   * Filter out static properties, which have a different syntax.
   */
  public function members() {
    return array_filter($this->properties(), function ($member) {
      return !$member->isStatic();
    });
  }

  protected function symbol($name) {
    return new PropertyName($this->refl, $name);
  }
}

/**
 * Static properties.
 */
class StaticProperties implements Source {
  use ReflectObjectProperties, StaticVisibility;
  
  protected function symbol($name) {
    return new StaticPropertyName($this->refl, $name);
  }
}

/**
 * Completion and info source for class constants.
 */
class ClassConstants implements Source {
  use SymbolSource, ReflectObject, CaseSensitive;

  protected function names() {
    return array_keys($this->refl->getConstants());
  }

  protected function symbol($name) {
    return new ClassConstant($this->refl, $name);
  }
}

/***********************************************************************
 *
 * Global completion sources across all classes/interfaces/traits.
 *
 * These are mostly useful for "apropos" functionality, rather than
 * tab-completion.
 *
 ***********************************************************************/

/**
 * Trait for combining across all defined classes/interfaces/traits
 */
trait ForAllClasses {
  abstract function source(Symbol $name);

  public function symbols() {
    $classlikes = new MergeSources(array(
      new ClassNames, new Interfaces, new Traits
    ));
    $value = array();
    foreach ($classlikes->symbols() as $class) {
      $source = $this->source($class);
      if ($source) {
        foreach ($source->symbols() as $symbol) {
          $value[] = $symbol;
        }
      }
    }
    return $value;
  }
}

class AllMethods implements Source {
  use CaseInsensitive, ForAllClasses;
  protected function source($name) {
    return new Methods((string) $name);
  }
}

class AllProperties implements Source {
  use CaseSensitive, ForAllClasses;
  protected function source($name) {
    return new Properties((string) $name);
  }
}

class AllStaticProperties implements Source {
  use CaseSensitive, ForAllClasses;
  protected function source($name) {
    return new StaticProperties((string) $name);
  }
}

class AllClassConstants implements Source {
  use CaseSensitive, ForAllClasses;
  protected function source($name) {
    return new ClassConstants((string) $name);
  }
}

class AllMembers extends MergeSources {
  public function __construct() {
    parent::__construct(array(
      new AllMethods,
      new AllProperties,
      new AllStaticProperties,
      new AllClassConstants,
    ));
  }
}

class AllSymbols extends MergeSources {
  public function __construct($scope = array()) {
    parent::__construct(array(
      new AllMembers,
      new Variables($scope),
      new Functions,
      new Constants,
      new Keywords,
      new ClassNames,
      new Interfaces,
      new Traits,
    ));
  }
}


/************************************************************
 *
 * Symbol types
 *
 ************************************************************/

/**
 * Annotation traits
 */

/**
 * Trait for symbol types with no annotation information
 */
trait AnnotateBasic {
  public function annotateBasic() {
    return array(
      'name' => (string) $this,
      'kind' => $this->kind(),
    );
  }

  public function annotate() {
    return $this->annotateBasic();
  }
}

trait AnnotateLocation {
  protected function annotateLocation(\Reflector $refl) {
    return array(
      'file' => $refl->getFileName(),
      'line' => $refl->getStartLine(),
    );
  }
}

trait AnnotateDocstring {
  protected function annotateDocstring(\Reflector $refl) {
    return array(
      'description' => self::getDocstring($refl),
    );
  }

  private static function getDocstring(\Reflector $refl) {
    $doc_comment = $refl->getDocComment();
    if ($doc_comment) {
      if (preg_match('|/[*][*]\s*\n\s*[*]\s*(.*)$|m', $doc_comment, $matches)) {
        return $matches[1];
      }
    }
  }
}

/**
 * Annotate property/method with declaring class
 */
trait AnnotateDeclaringClass {
  protected function annotateDeclaringClass(\Reflector $refl) {
    return array(
      'defined_in' => $refl->class,
    );
  }
}

/**
 * Trait for annotating function/method signatures using reflection.
 */
trait AnnotateSignature {
  protected function annotateSignature(\Reflector $refl) {
    return array(
      'arguments' => self::formatSignature($refl),
    );
  }

  protected static function formatSignature(\ReflectionFunctionAbstract $refl, $arg = -1) {
    $params   = $refl->getParameters();
    $n        = $refl->getNumberOfRequiredParameters();
    $required = array_slice($params, 0, $n);
    $optional = array_slice($params, $n);

    if (count($optional) && count($required)) {
      return sprintf('%s[, %s]',
      self::formatParams($required, $arg),
      self::formatParams($optional, $arg));
    } else if(count($required)) {
      return self::formatParams($required, $arg);
    } else if (count($optional)) {
      return sprintf('[%s]',
      self::formatParams($optional, $arg)); 
    } else {
      return '';
    }
  }

  protected static function formatParams(array $params, $index) {
    $args = array_map(function (\ReflectionParameter $param = NULL) use ($index) {
      try {
        $refl_class = $param->getClass();
        if($refl_class)
          $class = $refl_class->getName() . ' ';
        else
          $class = '';
      } catch (\ReflectionException $e) {
        $class = '';
      }
      $reference   = $param->isPassedByReference() ? '&' : '';
      $var         = $reference . $param->name;
      $highlighted = ($param->getPosition() == $index) ? strtoupper($var) : $var;
      $arg         = "{$class}\${$highlighted}";

      if($param->isDefaultValueAvailable()) {
        $default = var_export($param->getDefaultValue(), TRUE);
        $default = preg_replace('/\s+/', ' ', $default);
        return "{$arg} = {$default}";
      } else {
        return $arg;
      }
    }, $params);
    return implode(', ', $args);
  }
}

/**
 * Annotate class, interface or trait name by reflection
 */
trait AnnotateClass {
  use AnnotateBasic, AnnotateLocation, AnnotateDocstring;
  public function annotate() {
    $refl = new \ReflectionClass($this->name);
    return $this->annotateBasic($refl)
      + $this->annotateLocation($refl)
      + $this->annotateDocstring($refl);
  }
}

/************************************************************
 *
 * Traits for symbol formatting
 *
 ************************************************************/

trait PlainSymbol {
  public function __toString() {
    return $this->name;
  }
}

trait CallSymbol {
  public function __toString() {
    return $this->name . '(';
  }
}

trait DollarSymbol {
  public function __toString() {
    return '$' . $this->name;
  }
}

/************************************************************
 *
 * Symbols without an object/class context
 *
 ************************************************************/

trait SimpleSymbol {
  public function __construct($name) {
    $this->name = $name;
  }
}

// Variable
class Variable implements Symbol {
  use SimpleSymbol, DollarSymbol, AnnotateBasic;
  function kind() { return 'variable'; }
}

// Constant
class Constant implements Symbol {
  use SimpleSymbol, PlainSymbol, AnnotateBasic;
  function kind() { return 'constant'; }
}

// Keyword
class Keyword implements Symbol {
  use SimpleSymbol, PlainSymbol, AnnotateBasic;
  function kind() { return 'keyword'; }
}

// Function name
class FunctionName implements Symbol {
  use SimpleSymbol, CallSymbol, AnnotateBasic, AnnotateLocation, AnnotateDocstring, AnnotateSignature;

  function kind() { return 'function'; }
  function annotate() {
    $refl = new \ReflectionFunction($this->name);
    return $this->annotateBasic($refl)
      + $this->annotateLocation($refl)
      + $this->annotateDocstring($refl)
      + $this->annotateSignature($refl);
  }
}

// Plain class name
class ClassName implements Symbol {
  use SimpleSymbol, PlainSymbol, AnnotateClass;
  function kind() { return 'class'; }
}

// Interface name
class InterfaceName implements Symbol {
  use SimpleSymbol, PlainSymbol, AnnotateClass;
  function kind() { return 'interface'; }
}

// Trait name
class TraitName implements Symbol {
  use SimpleSymbol, PlainSymbol, AnnotateClass;
  function kind() { return 'trait'; }
}
    
// Class name used as constructor
class ClassConstructor implements Symbol {
  use SimpleSymbol, CallSymbol, AnnotateSignature;

  public function kind() { return 'class'; }
  public function annotate() {
    $refl = new \ReflectionClass($this->name);
    $info = $this->annotateBasic($refl);
    $constructor = $refl->getConstructor();
    if ($constructor) {
      $constructor_info = $this->annotateSignature($constructor);
      $info['arguments'] = $constructor_info['arguments'];
 
      if (empty($info['description'])) {
        $info['description'] = $constructor_info['description'];
      }
    }
    return $info;
  }
}

/************************************************************
 *
 * Symbols with an object/class context
 *
************************************************************/

trait ContextSymbol {
  private $context;
  private $name;
  function __construct($context, $name) {
    $this->context = $context;
    $this->name = $name;
  }
}

trait AnnotateMethod {
  use AnnotateBasic, AnnotateSignature, AnnotateDeclaringClass;
  function annotate() {
    $refl = $this->context->getMethod($this->name);
    return $this->annotateBasic($refl)
      + $this->annotateSignature($refl)
      + $this->annotateDeclaringClass($refl);
  }
}

trait AnnotateProperty {
  use AnnotateBasic, AnnotateDeclaringClass;
  function annotate() {
    $property = $this->context->getProperty($this->name);
    $class = $property->getDeclaringClass();
    $property_info = $this->annotateBasic($property);
    $class_info = $this->annotateBasic($class);
    $info = $class_info;
    $info['description'] = $property_info['description'];
    
    return $this->annotateDeclaringClass() + array(
      'file' => $class_info['file'],
      'line' => $class_info['line'],
      'description' => $property_info['description'],
    );
  }
}
  
class MethodName implements Symbol {
  use ContextSymbol, CallSymbol, AnnotateMethod;
  function kind() { return 'method'; }
}

class StaticMethodName implements Symbol {
  use ContextSymbol, CallSymbol, AnnotateMethod;
  function kind() { return 'static method'; }
}

class PropertyName implements Symbol {
  use ContextSymbol, PlainSymbol, AnnotateProperty;
  function kind() { return 'property'; }
}

class StaticPropertyName implements Symbol {
  use ContextSymbol, DollarSymbol, AnnotateProperty;
  function kind() { return 'static property'; }
}

class ClassConstant implements Symbol {
  use ContextSymbol, PlainSymbol, AnnotateBasic;
  function kind() { return 'class constant'; }
  function annotate() {
    $info = $this->annotateBasic($this->context);
    unset($info['description']);
    return $info;
  }
}
