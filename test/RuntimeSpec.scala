package test

import org.specs2.mutable._

import models.AST._

class RuntimeSpec extends Specification {
  
  "Runtime" should {
    
    "evaluate a simple numerical expression" in {
      val expression = ObjectExpression(Number_(2.0))
      expression.evaluate() must be equalTo Number_(2.0)
    }
    
    "evaluate a simple addition expression" in {
      val arguments = List(ObjectExpression(Number_(5.0)))
      val expression = Application(Reference("+", Some(ObjectExpression(Number_(23.0)))), arguments)
      expression.evaluate() must be equalTo Number_(28.0)
    }
    
    "evaluate a simple substraction expression" in {
      val arguments = List(ObjectExpression(Number_(6.0)))
      val expression = Application(Reference("-", Some(ObjectExpression(Number_(1.0)))), arguments)
      expression.evaluate() must be equalTo Number_(-5.0)
      println("Spec")
    }
  }
}