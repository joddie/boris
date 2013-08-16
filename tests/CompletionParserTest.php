<?php /* -*- compile-command: "phpunit CompletionParserTest" -*- */

class CompletionParserTest extends PHPUnit_Framework_TestCase
{
  public function setUp() {
    $this->parser = new \Boris\CompletionParser();
  }

  /**
   * Test basic matching functionality 
   */
  public function testMatch1() {
    $text = 'new stdClass(';
    $tokens = $this->parser->tokenize($text);

    /**
     * Matchers
     */
    $match = $this->parser->matchEnd($tokens, array(T_NEW, T_STRING, '('));
    $this->assertEquals($match, $tokens);
    $this->assertEquals($match[0]->text, 'new');
    $this->assertEquals($match[1]->text, 'stdClass');
    $this->assertEquals($match[2]->text, '(');

    $match = $this->parser->matchEnd($tokens, array(T_STRING, '('));
    $this->assertEquals($match, array_slice($tokens, 1));
    $this->assertEquals($match[0]->text, 'stdClass');
    $this->assertEquals($match[1]->text, '(');

    /**
     * Failers
     */
    $this->assertFalse($this->parser->matchEnd($tokens, array('=', T_NEW, T_STRING, '(')));
    $this->assertFalse($this->parser->matchEnd($tokens, array(T_VARIABLE)));
    $this->assertFalse($this->parser->matchEnd($tokens, array('[')));
  }

  /**
   * Test matching of qualified & unqualified names
   */
  public function testMatchQualifiedName() {
    $name = 'UnqualifiedName';
    $match = $this->parser->matchQualifiedName($this->parser->tokenize($name));
    $this->assertCount(1, $match);

    $name = 'function_name';
    $match = $this->parser->matchQualifiedName($this->parser->tokenize($name));
    $this->assertCount(1, $match);

    $name = '\stdClass';
    $match = $this->parser->matchQualifiedName($this->parser->tokenize($name));
    $this->assertCount(2, $match);

    $name = '\SomeNamespace\SomeClass';
    $match = $this->parser->matchQualifiedName($this->parser->tokenize($name));
    $this->assertCount(4, $match);

    $name = 'SomeNamespace\SubNamespace\SomeClass';
    $match = $this->parser->matchQualifiedName($this->parser->tokenize($name));
    $this->assertCount(5, $match);

    $name = '$foo, $bar = someName';
    $match = $this->parser->matchQualifiedName($this->parser->tokenize($name));
    $this->assertCount(1, $match);

    /**
     * Failers
     */
    $name = '$variable';
    $match = $this->parser->matchQualifiedName($this->parser->tokenize($name));
    $this->assertFalse($match);

    $name = 'function_call(';
    $match = $this->parser->matchQualifiedName($this->parser->tokenize($name));
    $this->assertFalse($match);
  }

  /**************************************************************
   *
   * Test partial line parsing for live documentation
   *
   *************************************************************/
  public function testMethodDocInfo() {
    $this->assertEquals($this->parser->getDocInfo('$x->y[2]->method'),
                        array(\Boris\CompletionParser::METHOD_INFO,
                              '$x->y[2]', FALSE,
                              'method'));
    $this->assertEquals($this->parser->getDocInfo('$x->y[2]->method ( '),
                        array(\Boris\CompletionParser::METHOD_INFO,
                              '$x->y[2]', FALSE,
                              'method'));
    $this->assertEquals($this->parser->getDocInfo('SomeClass::static_method'),
                        array(\Boris\CompletionParser::METHOD_INFO,
                              'SomeClass', TRUE,
                              'static_method'));
    $this->assertEquals($this->parser->getDocInfo('\Somewhere\SomeClass::static_method ( '),
                        array(\Boris\CompletionParser::METHOD_INFO,
                              '\Somewhere\SomeClass', TRUE,
                              'static_method'));
  }

  public function testClassDocInfo() {
    $this->assertEquals($this->parser->getDocInfo('new someClass'),
                        array(\Boris\CompletionParser::CLASS_INFO,
                              'someClass'));
    $this->assertEquals($this->parser->getDocInfo('new someClass( '),
                        array(\Boris\CompletionParser::CLASS_INFO,
                              'someClass'));
    $this->assertEquals($this->parser->getDocInfo('new \SomeOtherClass'),
                        array(\Boris\CompletionParser::CLASS_INFO,
                              '\SomeOtherClass'));
    $this->assertEquals($this->parser->getDocInfo('new \SomeOtherClass('),
                        array(\Boris\CompletionParser::CLASS_INFO,
                              '\SomeOtherClass'));
    $this->assertEquals($this->parser->getDocInfo('new \SomeNamespace\SomeOtherClass('),
                        array(\Boris\CompletionParser::CLASS_INFO,
                              '\SomeNamespace\SomeOtherClass'));
    $this->assertEquals($this->parser->getDocInfo('new SomeNamespace\SomeOtherClass('),
                        array(\Boris\CompletionParser::CLASS_INFO,
                              'SomeNamespace\SomeOtherClass'));
    $this->assertEquals($this->parser->getDocInfo('new \SomeNamespace\SubNamespace\SomeOtherClass'),
                        array(\Boris\CompletionParser::CLASS_INFO,
                              '\SomeNamespace\SubNamespace\SomeOtherClass'));
  }

  public function testFunctionDocInfo() {
    $this->assertEquals($this->parser->getDocInfo('some_function_call'),
                        array(\Boris\CompletionParser::FUNCTION_INFO,
                              'some_function_call'));
    $this->assertEquals($this->parser->getDocInfo('some_function_call('),
                        array(\Boris\CompletionParser::FUNCTION_INFO,
                              'some_function_call'));
    $this->assertEquals($this->parser->getDocInfo('\some_function_call('),
                        array(\Boris\CompletionParser::FUNCTION_INFO,
                              '\some_function_call'));
    $this->assertEquals($this->parser->getDocInfo('\SomeNamespace\some_function_call'),
                        array(\Boris\CompletionParser::FUNCTION_INFO,
                              '\SomeNamespace\some_function_call'));
    $this->assertEquals($this->parser->getDocInfo('SomeNamespace\SubNamespace\function_call('),
                        array(\Boris\CompletionParser::FUNCTION_INFO,
                              'SomeNamespace\SubNamespace\function_call'));
  }
  

  /**************************************************************
   *
   * Test partial parsing for completions
   *
   *************************************************************/

  private function lineCompletionInfo($line) {
    return $this->parser->getCompletionInfo($line, strlen($line));
  }

    public function testVar() {
        $line = '$variable';
        $this->assertEquals($this->lineCompletionInfo($line),
                            (object) array(
                              'how' => \Boris\CompletionParser::COMPLETE_VARIABLE,
                              'symbol' => '$variable',
                              'start' => 0,
                              'end' => 9));
    }

    public function testVarUnderscore() {
        $line = '$_var';
        $this->assertEquals($this->lineCompletionInfo($line),
                            (object) array(
                              'how' => \Boris\CompletionParser::COMPLETE_VARIABLE,
                              'symbol' => '$_var',
                              'start' => 0,
                              'end' => 5
                            ));
    }


    public function testVarMoreUnderscores() {
        $line = '$var1_with_more_words';
        $this->assertEquals($this->lineCompletionInfo($line),
                            (object) array(
                              'how' => \Boris\CompletionParser::COMPLETE_VARIABLE,
                              'symbol' => '$var1_with_more_words',
                              'start' => 0,
                              'end' => 21
                            ));
    }

    public function testDoubleQuoteIndex() {
        $line = '$variable["strin';
        $this->assertEquals($this->lineCompletionInfo($line),
                            (object) array(
                              'how' => \Boris\CompletionParser::COMPLETE_INDEX,
                              'base' => '$variable',
                              'symbol' => 'strin',
                              'start' => 11,
                              'end' => 16));
    }

    public function testMember() {
        $line = '$variable->member';
        $this->assertEquals($this->lineCompletionInfo($line),
                            (object) array(
                              'how' => \Boris\CompletionParser::COMPLETE_MEMBER,
                              'base' => '$variable',
                              'symbol' => 'member',
                              'start' => 11,
                              'end' => 17
                            ));
    }

    public function testMemberUnderscores() {
        $line = '$variable->member_with_more_words';
        $this->assertEquals($this->lineCompletionInfo($line),
                            (object) array(
                              'how' => \Boris\CompletionParser::COMPLETE_MEMBER,
                              'base' => '$variable',
                              'symbol' => 'member_with_more_words',
                              'start' => 11,
                              'end' => 33
                            ));
    }

    public function testMemberBeginningWithUnderscores() {
        $line = '$variable->__member_with_2_or_3_more_words';
        $this->assertEquals($this->lineCompletionInfo($line),
                            (object) array(
                              'how' => \Boris\CompletionParser::COMPLETE_MEMBER,
                              'base' => '$variable',
                              'symbol' => '__member_with_2_or_3_more_words',
                              'start' => 11,
                              'end' => 42
                            ));
    }


    public function testMemberWhitespace() {
        $line = '$variable 	-> 	member';
        $this->assertEquals($this->lineCompletionInfo($line),
                            (object) array(
                              'how' => \Boris\CompletionParser::COMPLETE_MEMBER,
                              'base' => '$variable',
                              'symbol' => 'member',
                              'start' => 15,
                              'end' => 21
                            ));
    }

    public function testDynamicMember() {
        $line = '$variable->$member';
        $this->assertEquals($this->lineCompletionInfo($line),
                            (object) array(
                              'how' => \Boris\CompletionParser::COMPLETE_VARIABLE,
                              'symbol' => '$member',
                              'start' => 11,
                              'end' => 18
                            ));
    }

    public function testSingleQuoteIndex() {
        $line = "\$variable['strin";
        $this->assertEquals($this->lineCompletionInfo($line),
                            (object) array(
                              'how' => \Boris\CompletionParser::COMPLETE_INDEX,
                              'base' => '$variable',
                              'symbol' => 'strin',
                              'start' => 11,
                              'end' => 16
                            ));
    }

    public function testSingleQuoteIndexWithEscapes() {
        $line = "\$variable['string with escapes: \\' \\\\ ";
        $this->assertEquals($this->lineCompletionInfo($line),
                            (object) array(
                              'how' => \Boris\CompletionParser::COMPLETE_INDEX,
                              'base' => '$variable',
                              'symbol' => 'string with escapes: \\\' \\\\ ',
                              'start' => 11,
                              'end' => 38
                            ));
    }

    public function testStaticMembers() {
      $line = '\Namespaced\ClassName::';
      $this->assertEquals($this->lineCompletionInfo($line),
                          (object) array(
                            'how' => \Boris\CompletionParser::COMPLETE_STATIC,
                            'base' => '\Namespaced\ClassName',
                            'is_bare' => true,
                            'symbol' => '',
                            'start' => 23,
                            'end' => 23));

        $line = 'ClassName::$static_member';
        $this->assertEquals($this->lineCompletionInfo($line),
                            (object) array(
                              'how' => \Boris\CompletionParser::COMPLETE_STATIC,
                              'base' => 'ClassName',
                              'is_bare' => true,
                              'symbol' => '$static_member',
                              'start' => 11,
                              'end' => 25
                            ));
    }

    public function testChains1() {
        $line = '$_some_var->member1->member2';
        $this->assertEquals($this->lineCompletionInfo($line),
                            (object) array(
                              'how' => \Boris\CompletionParser::COMPLETE_MEMBER,
                              'base' => '$_some_var->member1',
                              'symbol' => 'member2',
                              'start' => 21,
                              'end' => 28
                            ));

    }

    public function testChains2() {
        $line = '$_some_var["index"][123]->';
        $this->assertEquals($this->lineCompletionInfo($line),
                            (object) array(
                              'how' => \Boris\CompletionParser::COMPLETE_MEMBER,
                              'base' => '$_some_var["index"][123]',
                              'is_bare' => false,
                              'symbol' => '',
                              'start' => 26,
                              'end' => 26
                            ));
    }

    public function testNothingToComplete() {
        $line = '$_some_var->member -> other_member ["index"][123]';
        $this->assertNull($this->lineCompletionInfo($line));
    }

    public function testChains4() {
        $line = '$_some_var::$static_member->_member["index"][123]->';
        $this->assertEquals($this->lineCompletionInfo($line),
                            (object) array(
                              'how' => \Boris\CompletionParser::COMPLETE_MEMBER,
                              'base' => '$_some_var::$static_member->_member["index"][123]',
                              'is_bare' => false,
                              'symbol' => '',
                              'start' => 51,
                              'end' => 51
                            ));
    }

    public function testNoLeadingGarbage() {
        $line = 'some_function($something, $something_else';
        $this->assertEquals($this->lineCompletionInfo($line),
                            (object) array(
                              'how' => \Boris\CompletionParser::COMPLETE_VARIABLE,
                              'symbol' => '$something_else',
                              'start' => 26,
                              'end' => 41
                            ));

        $line = 'some_function($something->foo, $something_else["bar"]->xyz';
        $this->assertEquals($this->lineCompletionInfo($line),
                            (object) array(
                              'how' => \Boris\CompletionParser::COMPLETE_MEMBER,
                              'base' => '$something_else["bar"]',
                              'symbol' => 'xyz',
                              'start' => 55,
                              'end' => 58
                            ));
    }

}