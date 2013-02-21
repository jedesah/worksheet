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
    }
    
    "evaluate a simple boolean expression" in {
      val expression = ObjectExpression(Boolean_(true))
      expression.evaluate() must be equalTo Boolean_(true)
    }
    
    "evalate a simple boolean comparison expression" in {
      val arguments = List(ObjectExpression(Boolean_(false)))
      val expression = Application(Reference("==", None, Some(ObjectExpression(Boolean_(true)))), arguments)
      expression.evaluate() must be equalTo Boolean_(false)
    }
    
    "evaluate a simple function" in {
      val expression = ObjectExpression(new Function(Nil, Body(Nil, ObjectExpression(Number_(3)))))
      expression.evaluate() must be equalTo new Function(Nil, Body(Nil, ObjectExpression(Number_(3))))
    }
    
    "evaluate a simple function application" in {
      val function = ObjectExpression(new Function(Nil, Body(Nil, ObjectExpression(Number_(7)))))
      val application = Application(function)
      application.evaluate() must be equalTo Number_(7)
    }
    
    "evaluate a simple referenced application" in {
      val function = ObjectExpression(new Function(Nil, Body(Nil, ObjectExpression(Number_(90)))))
      val assignements = Map("gg" -> TypeMap(Map(None -> function)))
      val application = Application(Reference("gg"))
      application.evaluate(assignements) must be equalTo Number_(90)
    }
  }
}