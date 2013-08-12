<?php
class DummyClass {
    const CONSTANT = 12;
    const __ANOTHER_CONSTANT = "string";

    static $static_var_1 = 13;
    static $__static_var_2 = array(1, 2, 3);

    public function __construct() {
        $this->property = 9;
        $this->_private_prop = '';
        $this->__doubly_private = TRUE;
    }

    public function method() { return; }
    public function another_method() { return; }

    public static function static_method() { return 93; }
    public static function another_static_method() { return 93; }
}
