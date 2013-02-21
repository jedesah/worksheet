package test

import org.specs2.mutable._

import models.LanguageAST._

class InterpreterSpec extends Specification {
  
  "Interpreter" should {
    
    "evaluate a simple numerical expression" in {
      val expression = ObjectExpression(Number_(2.0))
      expression.evaluate() must be equalTo Number_(2.0)
    }
    
    "evaluate a simple addition expression" in {
      val arguments = List(ObjectExpression(Number_(5.0)))
      val expression = Application(Reference("+", None, Some(ObjectExpression(Number_(23.0)))), arguments)
      expression.evaluate() must be equalTo Number_(28.0)
    }
    
    "evaluate a simple substraction expression" in {
      val arguments = List(ObjectExpression(Number_(6.0)))
      val expression = Application(Reference("-", None, Some(ObjectExpression(Number_(1.0)))), arguments)
      expression.evaluate() must be equalTo Number_(-5.0)
      println("Spec")
    }
    
    "evaluate a simple boolean expression" in {
      val expression = ObjectExpression(Boolean_(true))
      expression.evaluate() must be equalTo Boolean_(true)
    }
  }
}