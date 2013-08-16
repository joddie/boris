<?php
class DummyClass {
    const CONSTANT = 12;
    const __ANOTHER_CONSTANT = "string";

    static $static_var_1 = 13;
    private static $__static_var_2 = array(1, 2, 3);
   
    public $property;
    private $_private_prop;
    private $__doubly_private;

    public function __construct() {
        $this->property = 9;
        $this->_private_prop = '';
        $this->__doubly_private = TRUE;
    }

    public function method($foo, &$bar, $baz = NULL) { return 19; }
    public function another_method($quux) { return 12; }

    public static function static_method($x, $y) { return 42; }
    public static function another_static_method() { return 93; }
}
