package models.AST

trait Statement

case class Assignement(name: String, value: Expression) extends Statement

trait Value

trait Expression extends Statement with Value {
  def evaluate(assignements: Map[String, Value] = Map()): Value
}

case class UncompiledExpression(content: String) extends Expression {
  def evaluate(assignements: Map[String, Value]) =
    Parser.parseSingleValidStatement(content).asInstanceOf[Expression].evaluate(assignements)
}

case class ObjectExpression(object_ : Object_) extends Expression {
  def evaluate(assignements: Map[String, Value]): Object_ = object_
}

case class Reference(name: String, parent: Option[Expression] = None) extends Expression {
  def evaluate(assignements: Map[String, Value]) = {
    println("Reference")
    parent match {
      case Some(parent) => parent.evaluate(assignements) match {
	case obj: Object_     => println("1");obj.members.getOrElse(name, this)
	case expr: Expression => println("2");this
      }
      case None => println("3");assignements.applyOrElse(name, (key: String) => this)
    }
  }
}

object ExpressionImplicits {
  implicit def Object2Expr(value: Object_) = ObjectExpression(value)
  implicit def Value2Expr(value: Value): Expression = value match {
    case obj: Object_ => obj
    case expr: Expression => expr
  }
  implicit def ValueList2ExprList(value: List[Value]): List[Expression] = {
    println(value)
    for (element <- value) yield Value2Expr(element)
  }
}

case class Application(expr: Expression, arguments: List[Expression]) extends Expression {
  def evaluate(assignements: Map[String, Value]) = {
    println("Application")
    println(expr)
    val onWhichToApply = expr.evaluate(assignements)
    println("Application1")
    val resolvedArguments = arguments.map(_.evaluate(assignements))
    val hasUnresolvedArguments = resolvedArguments.exists(_.isInstanceOf[Expression])
    if (hasUnresolvedArguments)
      Application(ExpressionImplicits.Value2Expr(onWhichToApply), ExpressionImplicits.ValueList2ExprList(resolvedArguments))
    else {
      println("Application2")
      val argumentsAsObject = resolvedArguments.map(_.asInstanceOf[Object_])
      onWhichToApply match {
	case expr: Expression   => println(resolvedArguments.length);Application(expr, ExpressionImplicits.ValueList2ExprList(resolvedArguments))
	case function: Function => println("Application4");function.apply_(argumentsAsObject)
	case obj: Object_ => println("Application5");obj.members("()").asInstanceOf[Function].apply_(argumentsAsObject)
      }
    }
  }
}
    

trait Object_ extends Value {
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

case class Substraction(this_ : Object_) extends Method(this_) {
  def applyMethod(arguments: List[Object_]) = {
    val values = for( argument <- arguments) yield argument match {
      case Number_(value) => value
      case _ => throw new Exception()
    }
    Number_(values.head - values.tail.sum)
  }
  
  def members = Map()
}

case class Number_(value: Double) extends Object_
{
  def members = Map("+" -> Addition(this), "-" -> Substraction(this))
  override def toString = if (value.isValidInt) value.toInt.toString else value.toString
}

class UnexpectedEndOfLineError extends Exception
import scala.util.parsing.combinator._

object StatementParser extends RegexParsers with PackratParsers {

  val integer = """[1-9][0-9]*"""r
  
  val identifier = """[a-zA-Z+*-]([a-zA-Z0-9+*-]|_[a-zA-Z0-9])*"""r

  def reel = integer ~ "." ~ ("""[0-9][0-9]*"""r) ^^ {
    case (firstpart ~ "." ~ lastPart) => firstpart + "." + lastPart
  }

  def statement = assignement | expression
  
  def assignement = identifier ~ ":=" ~ expression ^^ {
    case (name ~ ":=" ~ expression) => Assignement(name, expression)
  }
  
  lazy val reference: PackratParser[Reference] = expression ~ "." ~ identifier ^^ {
    case (expression ~ "." ~ name) => Reference(name, Some(expression))
  } | identifier ^^ {
    case (name) => Reference(name)
  }
  
  lazy val expression: PackratParser[Expression] = function_call | number | reference
  
  def number = (reel | integer) ^^ (n => ObjectExpression(Number_(n.toDouble)))
  
  lazy val function_call: PackratParser[Application] = reference ~ ("(" ~> repsep(expression, ",") <~ ")") ^^ {
    case (reference ~ params) => Application(reference, params)
  } | expression ~ " " ~ identifier ~ " " ~ expression ^^ {
    case (expression ~ " " ~ identifier ~ " " ~ param) =>
    Application(Reference(identifier, Some(expression)), List(param))
  }
}

object Parser {
  def parse(content: String): List[Statement] = {
    val lines = content.split('\n')
    for (line <- lines if line != "") yield parseSingleStatement(line) match {
      case StatementParser.Success(stat, _) => stat
    }
  }.toList
  
  def parseSingleStatement(content: String) = {
    if (content.contains('\n')) throw new UnexpectedEndOfLineError
    StatementParser.parseAll(StatementParser.statement, content)
  }
  
  def parseSingleValidStatement(content: String) = {
    parseSingleStatement(content) match {
      case StatementParser.Success(stat, _) => stat
    }
  }
}

object WorkSheet {
  def computeResults(code: String): List[String] = {
  
    var assignements: Map[String, Value] = Map()
  
    code.split('\n').toList.map{ line =>
      if (line == "") ""
      else Parser.parseSingleStatement(line) match {
	case StatementParser.Success(ass: Assignement, _) => {
	  assignements = assignements + (ass.name -> ass.value.evaluate(assignements))
	  ass.name + " = " + ass.value.evaluate(assignements).toString
	}
	case StatementParser.Success(exp: Expression, _) => exp.evaluate(assignements) match {
	  case ref: Reference => s"invalid reference: ${ref.name}"
	  case obj: Object_ => obj.toString
	}
	case _ => "invalid expression"
      }
    }
  }
}