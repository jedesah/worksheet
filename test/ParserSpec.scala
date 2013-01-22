package test

import org.specs2.mutable._

import models.AST._

class ParserSpec extends Specification {
  "Parser" should {

    "parse a simple numerical expression" in {
      val expression = "32"
      Parser.parseSingleStatement(expression) must be equalTo NumberLiteral(32)
    }
    
    "parse a simple addition expression" in {
      val expression = "+(7, 4)"
      val actual = Parser.parseSingleStatement(expression)
      actual must be equalTo FunctionCall("+", List(NumberLiteral(7), NumberLiteral(4)))
    }
    
    "determine length of parentheses construction" in {
      val expression = "-(7,4)"
      val length = Parser.findLengthBeforeClosingParentheses(expression.drop(2))
      val actual = expression.slice(2, 2 + length)
      actual must be equalTo "7,4"
    }
  }
}