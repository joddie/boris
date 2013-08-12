<?php /* -*- compile-command: "phpunit EvalWorkerCompletionTest" -*- */
class EvalWorkerCompletionTest extends PHPUnit_Framework_TestCase
{
    public function setUp() {
        $this->worker = new \Boris\EvalWorker();

        /* Fixture for testing filtering */
        $this->candidates = array(
            0 => 'get_class(',
            1 => 'get_called_class(',
            2 => 'get_parent_class(',
            3 => 'get_included_files(',
            4 => 'get_required_files(',
            5 => 'get_class_vars(',
            6 => 'get_object_vars(',
            7 => 'get_class_methods(',
            8 => 'get_declared_classes(',
            9 => 'get_declared_interfaces(',
            10 => 'get_defined_functions(',
            11 => 'get_defined_vars(',
            12 => 'get_resource_type(',
            13 => 'get_loaded_extensions(',
            14 => 'get_extension_funcs(',
            15 => 'get_defined_constants(',
            16 => 'get_html_translation_table(',
            17 => 'get_current_user(',
            18 => 'get_cfg_var(',
            19 => 'get_magic_quotes_gpc(',
            20 => 'get_magic_quotes_runtime(',
            21 => 'get_include_path(',
            22 => 'get_meta_tags(',
            23 => 'get_headers(',
            24 => 'get_browser('
        );

        /* Dummy class and instance for testing introspection methods */
        require_once 'testcase.php';
        $this->dummy_instance = new DummyClass();
    }

    /* Test finding the completion base */
    public function testVar() {
        $line = '$variable';
        $this->assertEquals($this->worker->_getCompletionBase($line, strlen($line)),
                            array($line, FALSE));
    }

    public function testVarUnderscore() {
        $line = '$_var';
        $this->assertEquals($this->worker->_getCompletionBase($line, strlen($line)),
                            array($line, FALSE));
    }


    public function testVarMoreUnderscores() {
        $line = '$var1_with_more_words';
        $this->assertEquals($this->worker->_getCompletionBase($line, strlen($line)),
                            array($line, FALSE));
    }

    public function testDoubleQuoteIndex() {
        $line = '$variable["string"]';
        $this->assertEquals($this->worker->_getCompletionBase($line, strlen($line)),
                            array($line, FALSE));
    }

    public function testMember() {
        $line = '$variable->member';
        $this->assertEquals($this->worker->_getCompletionBase($line, strlen($line)),
                            array($line, FALSE));
    }

    public function testMemberUnderscores() {
        $line = '$variable->member_with_more_words';
        $this->assertEquals($this->worker->_getCompletionBase($line, strlen($line)),
                            array($line, FALSE));
    }

    public function testMemberBeginningWithUnderscores() {
        $line = '$variable->__member_with_2_or_3_more_words';
        $this->assertEquals($this->worker->_getCompletionBase($line, strlen($line)),
                            array($line, FALSE));
    }


    public function testMemberWhitespace() {
        $line = '$variable 	-> 	member';
        $this->assertEquals($this->worker->_getCompletionBase($line, strlen($line)),
                            array($line, FALSE));
    }

    public function testDynamicMember() {
        $line = '$variable->$member';
        $this->assertEquals($this->worker->_getCompletionBase($line, strlen($line)),
                            array($line, FALSE));
    }

    public function testSingleQuoteIndex() {
        $line = "\$variable['string']";
        $this->assertEquals($this->worker->_getCompletionBase($line, strlen($line)),
                            array($line, FALSE));
    }

    public function testSingleQuoteIndexWithEscapes() {
        $line = "\$variable['string with escapes: \\' \\\\ \\\\\\' ']";
        $this->assertEquals($this->worker->_getCompletionBase($line, strlen($line)),
                            array($line, FALSE));
    }

    public function testNumericIndex() {
        $line = '$variable[0142]';
        $this->assertEquals($this->worker->_getCompletionBase($line, strlen($line)),
                            array($line, FALSE));
    }

    public function testNumericIndexWhitespace() {
        $line = '$variable  [  0142 ]';
        $this->assertEquals($this->worker->_getCompletionBase($line, strlen($line)),
                            array($line, FALSE));
    }

    public function testStaticMember() {
        $line = 'ClassName::$static_member';
        $this->assertEquals($this->worker->_getCompletionBase($line, strlen($line)),
                            array($line, FALSE));
    }

    public function testChains1() {
        $line = '$_some_var->member1->member2';
        $this->assertEquals($this->worker->_getCompletionBase($line, strlen($line)),
                            array($line, FALSE));

    }

    public function testChains2() {
        $line = '$_some_var["index"][123]';
        $this->assertEquals($this->worker->_getCompletionBase($line, strlen($line)),
                            array($line, FALSE));
    }

    public function testChains3() {
        $line = '$_some_var->member -> other_member ["index"][123]';
        $this->assertEquals($this->worker->_getCompletionBase($line, strlen($line)),
                            array($line, FALSE));
    }

    public function testChains4() {
        $line = '$_some_var::$static_member->_member["index"][123]';
        $this->assertEquals($this->worker->_getCompletionBase($line, strlen($line)),
                            array($line, FALSE));
    }

    public function testChains5() {
        $line = '$_some_var[0][1][\'thing\']->member::$static_member["index"][123]';
        $this->assertEquals($this->worker->_getCompletionBase($line, strlen($line)),
                            array($line, FALSE));
    }

    public function testChains6() {
        $line = '$_some_var->x->$y->$z::$s[0]';
        $this->assertEquals($this->worker->_getCompletionBase($line, strlen($line)),
                            array($line, FALSE));
    }

    public function testNoLeadingGarbage() {
        $line = 'some_function($something, $something_else';
        $this->assertEquals($this->worker->_getCompletionBase($line, strlen($line)),
                            array('$something_else', FALSE));

        $line = 'some_function($something->foo, $something_else["bar"]->xyz';
        $this->assertEquals($this->worker->_getCompletionBase($line, strlen($line)),
                            array('$something_else["bar"]->xyz', FALSE));
    }

    /* Test filtering */
    public function testFilteringNoPrefix() {
        $this->assertEquals(
            $this->worker->_filterCompletions($this->candidates, ''),
            $this->candidates);
    }

    public function testFilteringWithPrefix() {
        $this->assertEquals(
            $this->worker->_filterCompletions($this->candidates, 'get_de'),
            array(
                0 => 'get_declared_classes(',
                1 => 'get_declared_interfaces(',
                2 => 'get_defined_functions(',
                3 => 'get_defined_vars(',
                4 => 'get_defined_constants('
            ));
    }

    public function testFilteringNoResults() {
        $this->assertEquals(
            $this->worker->_filterCompletions($this->candidates, 'mysql_'),
            array());
    }

    /* Test getting elements of classes & objects */
    public function testObjectMembers() {
        $members = $this->worker->_objectMembers($this->dummy_instance);
        sort($members);
        $this->assertEquals($members, array(
            0 => '__construct(',
            1 => '__doubly_private',
            2 => '__static_var_2',
            3 => '_private_prop',
            4 => 'another_method(',
            5 => 'another_static_method(',
            6 => 'method(',
            7 => 'property',
            8 => 'static_method(',
            9 => 'static_var_1'
        ));
    }

    public function testStaticMembers() {
        $expected = array(
            0 => '$__static_var_2',
            1 => '$static_var_1',
            2 => 'CONSTANT',
            3 => '__ANOTHER_CONSTANT',
            4 => 'another_static_method(',
            5 => 'static_method('
        );

        /* Test on instance */
        $members = $this->worker->_staticMembers($this->dummy_instance);
        sort($members);
        $this->assertEquals($members, $expected);

        /* Test on class name */
        $members = $this->worker->_staticMembers('DummyClass');
        sort($members);
        $this->assertEquals($members, $expected);
    }
}