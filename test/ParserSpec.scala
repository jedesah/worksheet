package test

import org.specs2.mutable._

import models.AST._

class ParserSpec extends Specification {
  "Parser" should {

    "parse a simple numerical expression such as \"32\"" in {
      val expression = "32"
      Parser.parseSingleValidStatement(expression) must be equalTo ObjectExpression(Number_(32))
    }
    
    def additionExpression(first: Double, second: Double) = {
      val params = List(ObjectExpression(Number_(second)))
      Application(Reference("+", Some(ObjectExpression(Number_(first)))), params)
    }
    
    "parse a simple addition expression such as \"7.+(4)\"" in {
      val expression = "7.+(4)"
      val actual = Parser.parseSingleValidStatement(expression)
      actual must be equalTo additionExpression(7, 4)
    }
    
    "parse an infix operator notation addition expression such as \"7 + 4\"" in {
      val expression = "7 + 4"
      val actual = Parser.parseSingleValidStatement(expression)
      actual must be equalTo additionExpression(7, 4)
     }
  }
}