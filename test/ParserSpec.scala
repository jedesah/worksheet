package test

import org.specs2.mutable._

import models.AST._

class ParserSpec extends Specification {
  "Parser" should {

    "parse a simple numerical expression" in {
      val expression = "32"
      Parser.parseSingleStatement(expression) must be equalTo ObjectExpression(Number_(32))
    }
    
    "parse a simple addition expression" in {
      val expression = "7.+(4)"
      val actual = Parser.parseSingleStatement(expression)
      val expectedArguments = List(ObjectExpression(Number_(7)), ObjectExpression(Number_(4)))
      actual must be equalTo Application(Reference("+"), expectedArguments)
    }
  }
}