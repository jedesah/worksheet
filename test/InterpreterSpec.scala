package test

import org.specs2.mutable._

import models.LanguageAST._
import models.LanguageAST.ExpressionImplicits._

import Utils._

class InterpreterSpec extends Specification {
  
  "Interpreter" should {
    
    "evaluate a simple numerical expression" in {
      val expression = ObjectExpression(Number_(2.0))
      expression.evaluate() must be equalTo Number_(2.0)
    }
    
    "evaluate a simple addition expression" in {
      simpleMethodCall(5.0, 23.0, "+").evaluate() must be equalTo Number_(28.0)
    }
    
    "evaluate a simple substraction expression" in {
      simpleMethodCall(1.0, 6.0, "-").evaluate() must be equalTo Number_(-5.0)
    }
    
    "evaluate a simple boolean expression" in {
      val expression = ObjectExpression(Boolean_(true))
      expression.evaluate() must be equalTo Boolean_(true)
    }
    
    "evaluate a simple boolean comparison expression" in {
      simpleMethodCall(true, false, "==").evaluate() must be equalTo Boolean_(false)
    }
    
    "evaluate a simple function" in {
      val expression = ObjectExpression(new Function(Block(ObjectExpression(Number_(3)))))
      expression.evaluate() must be equalTo new Function(Block(ObjectExpression(Number_(3))))
    }
    
    "evaluate a simple function application" in {
      val function = ObjectExpression(new Function(Block(ObjectExpression(Number_(7)))))
      val application = Application(function)
      application.evaluate() must be equalTo Number_(7)
    }
    
    "evaluate a simple referenced functon application" in {
      val function = ObjectExpression(new Function(Block(ObjectExpression(Number_(90)))))
      val assignements = Map("gg" -> TypeMap(Map(None -> function)))
      val application = Application(Reference("gg"))
      application.evaluate(assignements) must be equalTo Number_(90)
    }
    
    "evaluate a simple parameterized function" in {
      val function = new Function(Block(Reference("toEcho")), List(Param("toEcho")))
      val expression = ObjectExpression(function)
      expression.evaluate() must be equalTo function
    }
    
    "evaluate a simple parameterized function application" in {
      val function = ObjectExpression(new Function(Block(Reference("toEcho")), List(Param("toEcho"))))
      val echo = Number_(9)
      val application = Application(function, List(ObjectExpression(echo)))
      application.evaluate() must be equalTo echo
    }
    
    "evaluate a simple referenced parameterized function application" in {
      val function = ObjectExpression(new Function(Block(Reference("toEcho")), List(Param("toEcho"))))
      val assignements = Map("bb" -> TypeMap(Map(None -> function)))
      val echo = Boolean_(true)
      val application = Application(function, List(ObjectExpression(echo)))
      application.evaluate(assignements) must be equalTo echo
    }
    
    "evaluate a simple positive if expression" in {
      val body = Block(ObjectExpression(Number_(-75.78)))
      val condition = ObjectExpression(Boolean_(true))
      val expression = IfExpression(condition, body)
      expression.evaluate() must be equalTo Number_(-75.78)
    }
    
    "evaluate a simple negative if expression" in {
      val body = Block(ObjectExpression(Number_(6)))
      val condition = ObjectExpression(Boolean_(false))
      val expression = IfExpression(condition, body)
      expression.evaluate() must be equalTo nothing
    }
    
    "evaluate a complex positive if expression" in {
      val body = Block(UncompiledExpression("10.+(3)"))
      val assignements = Map("proceed" -> TypeMap(Map(None -> Boolean_(true))))
      val condition = Reference("proceed")
      val expression = IfExpression(condition, body)
      expression.evaluate(assignements) must be equalTo Number_(13)
    }
    
    "evaluate a complex negative if expression" in {
      val body = Block(UncompiledExpression("13.+(1)"))
      val assignements = Map("proceed" -> TypeMap(Map(None -> Boolean_(false))))
      val condition = Reference("proceed")
      val expression = IfExpression(condition, body)
      expression.evaluate(assignements) must be equalTo nothing
    }
    
    "ignore the body of a negative if expression" in {
      val body = Block(UncompiledExpression(";^%$:"))
      val assignements = Map("proceed" -> TypeMap(Map(None -> Boolean_(false))))
      val condition = Reference("proceed")
      val expression = IfExpression(condition, body)
      expression.evaluate(assignements) must be equalTo nothing
    }
    
    "evaluate a simple positive if else expression" in {
      val ifBody = Block(ObjectExpression(Number_(1)))
      val elseBody = Block(ObjectExpression(Number_(3)))
      val condition = ObjectExpression(Boolean_(true))
      val expression = IfExpression(condition, ifBody, Some(elseBody))
      expression.evaluate() must be equalTo Number_(1)
    }
    
    "evaluate a simple negative if else expression" in {
      val ifBody = Block(ObjectExpression(Number_(1)))
      val elseBody = Block(ObjectExpression(Number_(3)))
      val condition = ObjectExpression(Boolean_(false))
      val expression = IfExpression(condition, ifBody, Some(elseBody))
      expression.evaluate() must be equalTo Number_(3)
    }
    
    "ignore the else body of a positive if else expression" in {
      val ifBody = Block(ObjectExpression(Number_(3)))
      val elseBody = Block(UncompiledExpression("j jj j ;;"))
      val condition = ObjectExpression(Boolean_(true))
      val expression = IfExpression(condition, ifBody, Some(elseBody))
      expression.evaluate() must be equalTo Number_(3)
    }
  }
}