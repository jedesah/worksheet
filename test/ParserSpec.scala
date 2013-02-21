package test

import org.specs2.mutable._

import models.LanguageAST._
import models.Parser

class ParserSpec extends Specification {
  "Parser" should {

    "parse a simple numerical expression such as \"32\"" in {
      val expression = "32"
      Parser.parseSingleValidStatement(expression) must be equalTo ObjectExpression(Number_(32))
    }
    
    def doubleAdditionExpr(first: Double, second: Double): Application = {
      val params = List(ObjectExpression(Number_(second)))
      Application(Reference("+", None, Some(ObjectExpression(Number_(first)))), params)
    }
    
    def refAdditionExpr(first: String, second: String): Application =
      refMethodCall(first, second, "+")
    
    def refMethodCall(first: String, second: String, operationName: String) = {
      val params = List(Reference(second, None))
      Application(Reference(operationName, None, Some(Reference(first, None))), params)
    }
    
    "parse a simple addition expression such as \"7.+(4)\"" in {
      val expression = "7.+(4)"
      val actual = Parser.parseSingleValidStatement(expression)
      actual must be equalTo doubleAdditionExpr(7.0, 4.0)
    }
    
    "parse a simple addition expression with references such as \"b.+(c)\"" in {
      val expression = "b.+(c)"
      val actual = Parser.parseSingleValidStatement(expression)
      actual must be equalTo refAdditionExpr("b", "c")
    }
    
    "parse a simple substraction expression with references such as \"b.-(c)\"" in {
      val expression = "b.-(c)"
      val actual = Parser.parseSingleValidStatement(expression)
      actual must be equalTo refMethodCall("b", "c", "-")
    }
    
    "parse an infix operator notation addition expression such as \"7 + 4\"" in {
      val expression = "7 + 4"
      val actual = Parser.parseSingleValidStatement(expression)
      actual must be equalTo doubleAdditionExpr(7, 4)
    }
     
    "parse multiple statements" in {
      val code = """a := b.+(c)
b := 10
c := 5
a"""
      val actual = Parser.parse(code)
      val statement1 = Assignement(Reference("a"), refAdditionExpr("b", "c"))
      val statement2 = Assignement(Reference("b"), ObjectExpression(Number_(10)))
      val statement3 = Assignement(Reference("c"), ObjectExpression(Number_(5)))
      val statement4 = Reference("a", None)
      val expected = List(statement1, statement2, statement3, statement4)
      actual must be equalTo expected
    }
    
    "parse the simple \"true\" expression" in {
      val code = "true"
      
      val actual = Parser.parseSingleValidStatement(code)
      val expected = ObjectExpression(Boolean_(true))
      actual must be equalTo expected
    }
    
    "parse the simple \"false\" expression" in {
      val code = "false"
      
      val actual = Parser.parseSingleValidStatement(code)
      val expected = ObjectExpression(Boolean_(false))
      actual must be equalTo expected
    }
    
    "parse a comparison between true and false" in {
      val code = "true.==(false)"
      
      val actual = Parser.parseSingleValidStatement(code)
      val params = List(ObjectExpression(Boolean_(false)))
      val expected = Application(Reference("==", None, Some(ObjectExpression(Boolean_(true)))), params)
      actual must be equalTo expected
    }
  }
}