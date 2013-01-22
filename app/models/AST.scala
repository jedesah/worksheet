package models.AST

trait Statement

trait Expression extends Statement with Result {
  def evaluate(assignements: Map[String, Expression] = Map()): Result
}

trait SimpleExpression extends Expression {
  override def evaluate(assignements: Map[String, Expression]) = this
}

case class UncompiledExpression(content: String) extends Expression {
  def evaluate(assignements: Map[String, Expression]) = {
    Parser.parseSingleStatement(content).asInstanceOf[Expression].evaluate(assignements)
  }
}

case class Assignement(name: String, value: Expression) extends Statement

case class FunctionCall(name: String, arguments: List[Expression]) extends Expression {
  def evaluate(assignements: Map[String, Expression]): Result = {
    val function = assignements(name)
    function match {
      case Addition => {
	val value = (arguments map { argument: Expression =>
	  argument match {
	    case NumberLiteral(x) => x
	    case _ => 0
	  }
	}).sum
	NumberLiteral(value)
      }
      case UserFunction(arguments, content) => content.last.asInstanceOf[Expression].evaluate()
      case _ => new Error
    }
  }
}
case class NumberLiteral(value: Double) extends SimpleExpression
case class Reference(name: String) extends Expression {
  def evaluate(assignements: Map[String, Expression]) = assignements(name)
}
trait Function extends SimpleExpression
case class UserFunction(arguments: List[Argument], content: List[Statement]) extends Function
object Addition extends Function

case class Argument(name: String)

trait Result
class Error extends Result

object Parser {
  def parse(content: String): List[Statement] = {
    val lines = content.split('\n')
    for (line <- lines) yield parseSingleStatement(line)
  }.toList
      
  def parseSingleStatement(content: String): Statement = {
    if (content.contains('\n')) throw new UnexpectedEndOfLineError
    else {
      val words = content.split(' ')
      if (words.length == 1) {
	try {
	  NumberLiteral(words.head.toDouble)
	} catch {
	  case _: NumberFormatException => Reference(words.head)
	}
      }
      else if (words(1) == ":=") Assignement(words(0), UncompiledExpression(words.drop(2).mkString(" ")))
      else {
	if (words.head.contains('(')) {
	  val (name, rest) = content.span(_ != '(')
	  val paramsString = rest.drop(1)
	  val length = findLengthBeforeClosingParentheses(paramsString)
	  val parameters = paramsString.take(length).split(',').map( argExprString => parseSingleStatement(argExprString.trim).asInstanceOf[Expression]).toList
	  FunctionCall(name, parameters)
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

object Runtime {
 
  val baseLanguage = Map("+" -> Addition)

  def evaluate(expr: Expression) = expr.evaluate(baseLanguage)
}