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
    
    "evaluate a simple boolean comparison expression" in {
      val arguments = List(ObjectExpression(Boolean_(false)))
      val expression = Application(Reference("==", None, Some(ObjectExpression(Boolean_(true)))), arguments)
      expression.evaluate() must be equalTo Boolean_(false)
    }
    
    "evaluate a simple function" in {
      val expression = ObjectExpression(new Function(Body(ObjectExpression(Number_(3)))))
      expression.evaluate() must be equalTo new Function(Body(ObjectExpression(Number_(3))))
    }
    
    "evaluate a simple function application" in {
      val function = ObjectExpression(new Function(Body(ObjectExpression(Number_(7)))))
      val application = Application(function)
      application.evaluate() must be equalTo Number_(7)
    }
    
    "evaluate a simple referenced functon application" in {
      val function = ObjectExpression(new Function(Body(ObjectExpression(Number_(90)))))
      val assignements = Map("gg" -> TypeMap(Map(None -> function)))
      val application = Application(Reference("gg"))
      application.evaluate(assignements) must be equalTo Number_(90)
    }
    
    "evaluate a simple parameterized function" in {
      val function = new Function(Body(Reference("toEcho")), List(Param("toEcho")))
      val expression = ObjectExpression(function)
      expression.evaluate() must be equalTo function
    }
    
    "evaluate a simple parameterized function application" in {
      val function = ObjectExpression(new Function(Body(Reference("toEcho")), List(Param("toEcho"))))
      val echo = Number_(9)
      val application = Application(function, List(ObjectExpression(echo)))
      application.evaluate() must be equalTo echo
    }
    
    "evaluate a simple referenced parameterized function application" in {
      val function = ObjectExpression(new Function(Body(Reference("toEcho")), List(Param("toEcho"))))
      val assignements = Map("bb" -> TypeMap(Map(None -> function)))
      val echo = Boolean_(true)
      val application = Application(function, List(ObjectExpression(echo)))
      application.evaluate(assignements) must be equalTo echo
    }
  }
}