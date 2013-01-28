package models.AST

trait Statement

case class Assignement(name: String, value: Expression) extends Statement

trait Expression extends Statement {
  def evaluate(assignements: Map[String, Object_] = Map()): Object_
}

case class UncompiledExpression(content: String) extends Expression {
  def evaluate(assignements: Map[String, Object_]) = {
    Parser.parseSingleStatement(content).asInstanceOf[Expression].evaluate(assignements)
  }
}

case class ObjectExpression(object_ : Object_) extends Expression {
  def evaluate(assignements: Map[String, Object_]) = object_
}

case class Reference(name: String, parent: Option[Expression] = None) extends Expression {
  def evaluate(assignements: Map[String, Object_]) = {
    parent match {
      case Some(parent) => parent.evaluate(assignements).members(name)
      case None => assignements(name)
    }
  }
}

case class Application(reference: Expression, arguments: List[Expression]) extends Expression {
  def evaluate(assignements: Map[String, Object_]) = {
    reference.evaluate(assignements) match {
      case function: Function => function.apply_(arguments.map(_.evaluate(assignements)))
      case object_ => object_.members("()").asInstanceOf[Function].apply_(arguments.map(_.evaluate(assignements)))
    }
  }
}

trait Object_ {
  def members: Map[String, Object_]
}

trait Function extends Object_ {
  def apply_(arguments: List[Object_]): Object_
}

abstract class Method(this_ : Object_) extends Function {
  def apply_(arguments: List[Object_]) = applyMethod(this_ :: arguments)
  def applyMethod(arguments: List[Object_]): Object_
}

case class Addition(this_ : Object_) extends Method(this_) {
  def applyMethod(arguments: List[Object_]) = {
    val values = for( argument <- arguments) yield argument match {
      case Number_(value) => value
      case _ => throw new Exception()
    }
    Number_(values.sum)
  }
  
  def members = Map()
}
case class Number_(value: Double) extends Object_
{
  def members = Map("+" -> Addition(this))
  override def toString = value.toString
}

object Parser {
  def parse(content: String): List[Statement] = {
    val lines = content.split('\n')
    for (line <- lines if line != "") yield parseSingleStatement(line)
  }.toList
      
  def parseSingleStatement(content: String): Statement = {
    if (content.contains('\n')) throw new UnexpectedEndOfLineError
    else {
      val words = content.split(' ')
      if (words.length == 1) {
	try {
	  ObjectExpression(Number_(words.head.toDouble))
	} catch {
	  case _: NumberFormatException => Reference(words.head)
	}
      }
      else if (words(1) == ":=") Assignement(words(0), parseSingleStatement(words.drop(2).mkString(" ")).asInstanceOf[Expression])
      else {
	if (words.head.contains('(')) {
	  val (name, rest) = content.span(_ != '(')
	  val paramsString = rest.drop(1)
	  val length = findLengthBeforeClosingParentheses(paramsString)
	  val parameters = paramsString.take(length).split(',').map( argExprString => parseSingleStatement(argExprString.trim).asInstanceOf[Expression]).toList
	  Application(Reference(name), parameters)
	}
	else throw UnexpectedTokenError(words.tail.toList)
      }
    }
  }
  
  def findLengthBeforeClosingParentheses(content: String): Int = {
    var stack = 0
    for ((ch, i) <- content.zipWithIndex) {
      if (ch == ')') {
	if (stack == 0) {
	  return i
	}
	else stack = stack - 1
      }
      else if (ch == '(') stack = stack + 1
    }
    throw UnclosedParenthesesError(stack + 1)
  }
    
  case class UnclosedParenthesesError(amount: Int) extends Exception
  class UnexpectedEndOfLineError extends Exception
  case class UnexpectedTokenError(tokens: List[String]) extends Throwable
}

object WorkSheet {
  def computeResults(code: String): List[String] = {
    val statements = Parser.parse(code)
    
    var assignements: Map[String, Object_] = Map()
    
    for (statement <- statements) yield {
      statement match {
	case x: Assignement => {
	  assignements = assignements + (x.name -> x.value.evaluate(assignements))
	  x.name + " = " + x.value.evaluate(assignements).toString
	}
	case x: Expression => x.evaluate(assignements).toString
      }
    }
  }
}