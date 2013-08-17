<?php

namespace Boris;

/**
 * CompletionParser tokenizes and matches fragments of code in order
 * to provide intelligent completions and help lookup.
 */
class CompletionParser {

  const METHOD_INFO = 0;
  const FUNCTION_INFO = 1;
  const CLASS_INFO = 2;

  const COMPLETE_MEMBER = 0;
  const COMPLETE_STATIC = 1;
  const COMPLETE_VARIABLE = 2;
  const COMPLETE_INDEX = 3;
  const COMPLETE_CLASS = 4;
  const COMPLETE_SYMBOL = 5;

  /**
   * Partial parse for completion info
   */
  public function getCompletionInfo($input) {
    $tokens = $this->tokenize($input);
    if(count($tokens) === 0) {
      $end = strlen($input);
      return (object) array('how' => self::COMPLETE_SYMBOL,
                            'symbol' => '',
                            'start' => $end, 'end' => $end);
    }
    elseif (($match = $this->popMatch($tokens, array(T_OBJECT_OPERATOR)))
        || ($match = $this->popMatch($tokens, array(T_DOUBLE_COLON)))) {
      $operator = $match[0];
      list($base, $is_bare) = $this->getBaseTokens($tokens);
      if($operator->type == T_OBJECT_OPERATOR) $how = self::COMPLETE_MEMBER;
      else $how = self::COMPLETE_STATIC;
      $end = strlen($input);
      return (object) array('how' => $how,
                            'base' => $base, 'is_bare' => $is_bare,
                            'symbol' => '',
                            'start' => $end, 'end' => $end);
    }
    elseif (($matches = $this->popMatch($tokens, array(T_OBJECT_OPERATOR, T_STRING)))) {
      $symbol = $matches[1];
      list($base, $is_bare) = $this->getBaseTokens($tokens);
      if($is_bare) return NULL;
      return (object) array('how' => self::COMPLETE_MEMBER,
                            'base' => $base, 'is_bare' => FALSE,
                            'symbol' => $symbol->text,
                            'start' => $symbol->start, 'end' => $symbol->end);
    }
    elseif   (($matches = $this->popMatch($tokens, array(T_DOUBLE_COLON, T_STRING)))
              || ($matches = $this->popMatch($tokens, array(T_DOUBLE_COLON, T_VARIABLE)))) {
      $symbol = $matches[1];
      list($base, $is_bare) = $this->getBaseTokens($tokens);
      return (object) array('how' => self::COMPLETE_STATIC,
                            'base' => $base, 'is_bare' => $is_bare,
                            'symbol' => $symbol->text,
                            'start' => $symbol->start, 'end' => $symbol->end);
    }
    elseif (($matches = $this->popMatch($tokens, array(T_VARIABLE)))) {
      $symbol = $matches[0];
      return (object) array('how' => self::COMPLETE_VARIABLE,
                            'symbol' => $symbol->text,
                            'start' => $symbol->start, 'end' => $symbol->end);
    }
    elseif (($matches = $this->popMatch($tokens, array('[', T_STRING)))
            || ($matches = $this->popMatch($tokens, array('[', T_ENCAPSED_AND_WHITESPACE)))
            || ($matches = $this->popMatch($tokens, array('[', '"', T_ENCAPSED_AND_WHITESPACE)))) {
      $symbol = $matches[count($matches) - 1];
      /* Bit of a hack, here. */
      if($symbol->type == T_ENCAPSED_AND_WHITESPACE && count($matches) == 2) {
        /* Strip off the beginning single quote */
        $symbol->start += 1;
        $symbol->text = substr($symbol->text, 1);
      }
      list($base, $is_bare) = $this->getBaseTokens($tokens);
      if($is_bare) return NULL;
      return (object) array('how' => self::COMPLETE_INDEX,
                            'base' => $base,
                            'symbol' => $symbol->text,
                            'start' => $symbol->start, 'end' => $symbol->end);
    }
    elseif (($matches = $this->popMatch($tokens, array(T_NEW)))) {
      $end = strlen($input);
      return (object) array('how' => self::COMPLETE_CLASS,
                            'symbol' => '',
                            'start' => $end, 'end' => $end);
    }
    elseif (($matches = $this->popQualifiedName($tokens, TRUE))) {
      if($new = $this->popMatch($tokens, array(T_NEW))) {
        $how = self::COMPLETE_CLASS;
      } else {
        $how = self::COMPLETE_SYMBOL;
      }
      $name = $this->tokensText($matches);
      $bounds = $this->tokensBounds($matches);
      return (object) array('how' => $how,
                            'symbol' => $name,
                            'start' => $bounds[0],
                            'end' => $bounds[1]);
    }
    else {
      return NULL;
    }
  }


  /**
   * Partial parse for documentation lookup
   */
  public function getDocInfo($input) {
    $tokens = $this->tokenize($input);
    if ($this->matchEnd($tokens, array('('))) {
      array_pop($tokens);
    }

    if (($matches = $this->popMatch($tokens, array(T_OBJECT_OPERATOR, T_STRING)))
        || ($matches = $this->popMatch($tokens, array(T_DOUBLE_COLON, T_STRING)))) {
      list(, $method) = $matches;
      list($base, $bare) = $this->getBaseTokens($tokens);
      return array(self::METHOD_INFO, $base, $bare, $method->text);
    }
    elseif ($name = $this->popQualifiedName($tokens)) {
      if ($this->matchEnd($tokens, array(T_NEW))) {
        return array(self::CLASS_INFO, $this->tokensText($name));
      }
      else {
        return array(self::FUNCTION_INFO, $this->tokensText($name));
      }
    }
    else return FALSE;
  }


  /* private methods below */

  /**
   * Provide a nicer interface to PHP's built-in tokenizer, by
   * transforming its values into objects with a standard set of
   * fields and information about character start and end positions.
   */
  function tokenize($input) {
    static $dummy = '<?php ';

    $raw_tokens = token_get_all($dummy . $input);
    $pos = 0;
    $tokens = array();

    /* Begin loop at index 1, skipping the dummy <?php token */
    foreach (array_slice($raw_tokens, 1) as $raw_token) {
      $token = new \stdClass();
      if(is_string($raw_token)) {
        $token->text = $token->type = $token->name = $raw_token;
      } else {
        $token->type = $raw_token[0];
        $token->text = $raw_token[1];
        $token->name = token_name($token->type);
      }
      $token->start = $pos;
      $token->end = $token->start + strlen($token->text);
      $token->original_string = $input;
      $pos = $token->end;
      array_push($tokens, $token);
    }

    /* Remove whitespace and renumber indices from 0 */
    return array_values(array_filter($tokens, function($token) {
      return $token->type !== T_WHITESPACE;
    }));
  }


  private function tokensText($tokens) {
    if(!count($tokens)) return '';
    list($start, $end) = $this->tokensBounds($tokens);
    $text = $tokens[0]->original_string;
    return substr($text, $start, $end - $start);
  }

  private function tokensBounds($tokens) {
    if(!count($tokens)) throw new \RuntimeException("Asked for bounds of empty token list.");
    return array($tokens[0]->start,
                 $tokens[count($tokens) - 1]->end);
  }

  /*
   * Token pattern-matching
   */
  function matchEnd($tokens, $pattern) {
    if(count($pattern) > count($tokens)) return FALSE;
    $tokens = array_slice($tokens, -count($pattern));
    foreach(array_combine($pattern, $tokens) as $type => $token) {
      if($token->type != $type) return FALSE;
    }
    return $tokens;
  }

  function matchQualifiedName($tokens, $trailing_ok = FALSE) {
    $match = array();
    if($trailing_ok && $this->matchEnd($tokens, array(T_NS_SEPARATOR))) {
      array_unshift($match, array_pop($tokens));
    }

    while(TRUE) {
      if($this->matchEnd($tokens, array(T_STRING))) {
        array_unshift($match, array_pop($tokens));
      } else {
        break;
      }

      if($this->matchEnd($tokens, array(T_NS_SEPARATOR))) {
        array_unshift($match, array_pop($tokens));
      } else {
        break;
      }
    }
    if (count($match)) return $match;
    return FALSE;
  }

  private function popMatch(&$tokens, $pattern) {
    $match = $this->matchEnd($tokens, $pattern);
    if(!$match) return FALSE;
    array_splice($tokens, -count($match));
    return $match;
  }

  private function popQualifiedName(&$tokens, $trailing_ok = FALSE) {
    $match = $this->matchQualifiedName($tokens, $trailing_ok);
    if(!$match) return FALSE;
    array_splice($tokens, -count($match));
    return $match;
  }

  /**
   * Get the portion of code which must be evaluated to perform
   * tab-completion.
   *
   * Example: if the cursor is at the end of the following line:
   *   printf("%d\n", $arr['index'][123]->member->x_
   * then the portion of the line which needs evaluation is
   *   $arr['index'][123]->member
   *
   * The getCompletionInfo method takes care of matching and removing
   * portion at the end of the line ("->x_" in the example above).
   * This function works by scanning backward from just before that
   * fragment, accepting anything that looks like either a constant
   * array index or a property lookup.
   */
  private function getBaseTokens ($tokens) {
    $match = array();
    $is_bare = FALSE;
    while(count($tokens)) {
      if(($part = $this->popMatch($tokens, array(T_OBJECT_OPERATOR, T_STRING)))
         || ($part = $this->popMatch($tokens, array(T_OBJECT_OPERATOR, T_VARIABLE)))
         || ($part = $this->popMatch($tokens, array(T_DOUBLE_COLON, T_STRING)))
         || ($part = $this->popMatch($tokens, array(T_DOUBLE_COLON, T_VARIABLE)))
         || ($part = $this->popMatch($tokens, array('[', T_LNUMBER, ']')))
         || ($part = $this->popMatch($tokens, array('[', T_STRING , ']')))
         || ($part = $this->popMatch($tokens, array('[', T_CONSTANT_ENCAPSED_STRING , ']')))) {
        $match = array_merge($part, $match);
      }
      elseif($part = $this->popMatch($tokens, array(T_VARIABLE))) {
        /* Matched a variable name at the beginning of the expression. */
        $match = array_merge($part, $match);
        $is_bare = FALSE;
        break;
      }
      elseif($part = $this->popQualifiedName($tokens)) {
        /* Matched a bare name at the beginning of the expression.
         *
         * If the bare name is the entire 'expression', it's a class
         * or interface name which should be passed as a string to the
         * reflection methods, not evaluated. */
        $is_bare = count($match) == 0;
        $match = array_merge($part, $match);
        break;
      }
      else {
        /* Found something else.  Assume we don't want to evaluate it
         * for tab-completion (function/method calls, etc.)
         */
        $match = NULL;
        break;
      }
    }

    if(count($match)) {
      return array($this->tokensText($match), $is_bare);
    }
    else return FALSE;
  }

}
