package test

import models.LanguageAST._

object Utils {
  def simpleMethodCall(first: Double, second: Double, operationName: String): Application =
    simpleMethodCall(ObjectExpression(Number_(first)), ObjectExpression(Number_(second)), operationName)
    
  def simpleMethodCall(first: Boolean, second: Boolean, operationName: String): Application =
    simpleMethodCall(ObjectExpression(Boolean_(first)), ObjectExpression(Boolean_(second)), operationName)
  
  def simpleMethodCall(first: String, second: String, operationName: String): Application =
    simpleMethodCall(Reference(first), Reference(second), operationName) 
  
  def simpleMethodCall(first: Expression, second: Expression, operationName: String) =
    Application(Reference(operationName, None, Some(first)), List(second))
}