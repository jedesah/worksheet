package test

import org.specs2.mutable._

import models.AST._

class RuntimeSpec extends Specification {
  
  "Runtime" should {
    
    "evaluate a simple numerical expression" in {
      val expression = NumberLiteral(2.0)
      expression.evaluate() must be equalTo(NumberLiteral(2.0))
    }
    
    "evaluate a simple addition expression" in {
      val expression = FunctionCall("+", List(NumberLiteral(23.0), NumberLiteral(5.0)))
      Runtime.evaluate(expression) must be equalTo(NumberLiteral(28.0))
    }
  }
}