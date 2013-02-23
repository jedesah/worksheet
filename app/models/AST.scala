package models

object LanguageAST {

  trait Statement
  
  case class UncompiledStatement(content: String) extends Statement

  case class Assignement(ref: Reference, value: Expression) extends Statement

  case class TypeMap(content: Map[Option[String], Value]) {
    def resolve(referredAs: Option[String], expected: Type): Value = {
      referredAs match {
	case None =>
	  if (content.size == 1) content.values.head
	  else
	    if (content.contains(None)) content(None)
	    else
	      // TODO: fix this
	      /*if (content.keys.count(_.subTypeOrSameTypeAs(expected)) == 1)
		content.find(_.subTypeOrSameTypeAs(expected)).get
	      else*/
		throw new AmbiguousReferenceError
	case Some(type_) =>
	  if (content.contains(Some(type_))) content(Some(type_))
	  else throw new InvalidReferenceError
      }
    }
  }

  class AmbiguousReferenceError extends Error
  class InvalidReferenceError extends Error

  trait Value {
    def evaluate(assignements: Map[String, TypeMap] = Map(), expected:Type = Any): Value
  }

  trait Expression extends Statement with Value

  case class UncompiledExpression(content: String) extends Expression {
    def evaluate(assignements: Map[String, TypeMap], expected:Type) =
      Parser.parseSingleValidStatement(content).asInstanceOf[Expression].evaluate(assignements)
  }

  case class AmbigiousExpression(possiblesExprs: List[Expression]) extends Expression {
    def evaluate(assignements: Map[String, TypeMap], expected:Type): Value = {
      for (expr <- possiblesExprs) {
	expr.evaluate(assignements) match {
	  case obj: Object_ => return obj
	  case _ => ;
	}
      }
      return this
    }
  }

  case class ParenthesisedExpression(innerExpr: Expression) extends Expression {
    def evaluate(assignements: Map[String, TypeMap], expected:Type) = innerExpr.evaluate(assignements)
    override def toString = "(" + innerExpr.toString + ")"
  }

  case class ObjectExpression(object_ : Object_) extends Expression {
    def evaluate(assignements: Map[String, TypeMap], expected:Type): Object_ = object_
    override def toString = object_.toString
  }

  case class Reference(name: String, type_ : Option[String] = None, parent: Option[Expression] = None) extends Expression {
    def evaluate(assignements: Map[String, TypeMap], expected:Type) = {
      parent match {
	case Some(parent) => parent.evaluate(assignements, expected) match {
	  case obj: Object_     => obj.members.getOrElse(name, this)
	  case expr: Expression => Reference(name, type_, Some(expr))
	}
	case None => try {
		      assignements(name).resolve(type_, expected).evaluate(assignements, expected)
		    } catch {
		      case _ : Throwable => this
		    }
      }
    }
    
    override def toString =
      parent.map(_.toString + ".").getOrElse("") + name + type_.map(":" + _.toString).getOrElse("")
  }

  object ExpressionImplicits {
    implicit def Object2Expr(value: Object_) = ObjectExpression(value)
    implicit def Value2Expr(value: Value): Expression = value match {
      case obj: Object_ => obj
      case expr: Expression => expr
    }
    implicit def ValueList2ExprList(value: List[Value]): List[Expression] = {
      for (element <- value) yield Value2Expr(element)
    }
    implicit def ExpressionToStatementList(value: Expression): List[Statement] = List(value)
  }
  
  import ExpressionImplicits._

  case class Application(expr: Expression, arguments: List[Expression] = Nil) extends Expression {
    def evaluate(assignements: Map[String, TypeMap], expected:Type) = {
      expr.evaluate(assignements, Any) match {
	case expr: Expression   => Application(expr, arguments.map(_.evaluate(assignements, Any)))
	case function: Function => function.tryApply(arguments, assignements)
	case obj: Object_ => obj.members("()").asInstanceOf[Function].tryApply(arguments, assignements)
      }
    }
    
    override def toString = expr.toString + "(" + arguments.mkString(",") + ")"
  }
      

  abstract class Object_(type_ : Type) extends Value {
    def members: Map[String, Object_] = Map()
    override def evaluate(assignements: Map[String, TypeMap], expected:Type) = this
  }

  class Function(val body: Block, val params: List[Param] = Nil) extends Object_(functionClass) {
    def apply_(arguments: List[Object_]): Object_ =
      if (params.length != arguments.length) this // TODO: Produce a partially applied function
      else {
	val assignements = params.zip(arguments).map{
	  case (param, object_) =>
	    (param.name, TypeMap(Map(param.type_ -> object_)))
	}.toMap
	body.evaluate(assignements) match {
	  case result: Object_ => result
	  case _ => throw new Error
	}
      }
    def resolve(arguments: List[Expression], assignements: Map[String, TypeMap]): List[Value] =
      arguments.zip(params.map(_.type_)).map{ case (expr, type_) => expr.evaluate(assignements, Any) }
    def canResolve(arguments: List[Expression]): Boolean = ???
    def tryApply(arguments: List[Expression], assignements: Map[String, TypeMap]): Value = {
      val resolvedArguments = resolve(arguments, assignements)
      if (resolvedArguments.forall(_.isInstanceOf[Object_]))
	apply_(resolvedArguments.map(_.asInstanceOf[Object_]))
      else
	throw new Error
    }
    override def equals(other: Any) =
      if (other == null) false
      else other match {
	case other: Function => params == other.params && body == other.body
	case _ => false
      }
    override def toString = "(" + params.mkString(",") + ") =>" 
  }

  case class Param(name: String, type_ : Option[String] = None)

  case class Block(content: List[Statement]) extends Expression {
    def evaluate(assignements: Map[String, TypeMap], expected:Type) =
      content.last match {
	case expr: Expression => {
	  var bodyAssignements = assignements
    
	  for (statement <- content.init) {
	    statement match {
	      case Assignement(ref, value) => {
		ref match {
		  case Reference(name, type_, None) =>
		    // TODO: factor in the declared type to help evaluate the expression
		    bodyAssignements = bodyAssignements + (name -> TypeMap(Map(type_ -> value.evaluate(bodyAssignements, Any))))
		  case Reference(name, type_, Some(expr)) =>
		    // if is assignable, modify variable in assignements to reflect this assignement
		}
	      }
	      case _ => ;
	    }
	  }
	  expr.evaluate(bodyAssignements)
	}
	case _ => nothing
      }
    def evaluateOutput(assignements: Map[String, TypeMap], expected:Type): List[String] = {
      var bodyAssignements = assignements
      
      for (statement <- content) yield {
	statement match {
	  case Assignement(ref, value) => {
	    ref match {
	      case Reference(name, type_, None) => {
		// TODO: factor in the declared type to help evaluate the expression
		val evaluatedExpr = value.evaluate(bodyAssignements, Any)
		bodyAssignements = bodyAssignements + (name -> TypeMap(Map(type_ -> evaluatedExpr)))
		name + " = " + evaluatedExpr.toString
	      }
	      case Reference(name, type_, Some(expr)) => "unsupported assignation"
		// if is assignable, modify variable in assignements to reflect this assignement
	    }
	  }
	  case ref: Reference => ref.evaluate(bodyAssignements) match {
	    case resultRef: Reference if (resultRef == ref) => s"invalid reference: ${ref.name}"
	    case value => value.toString
	  }
	  case expr: Expression => expr.evaluate(bodyAssignements).toString
	  case stat: UncompiledStatement => "invalid statement"
	}
      }
    }
  }
  
  trait Type {
    def subClassOrSameTypeAs(other:Type)
  }

  case class Struct(composition: Map[String, Option[Class]] = Map()) extends Type {
    def subClassOrSameTypeAs(other: Type) = ???
  }

  abstract class Class(val name: String, val struct : Struct) extends Type {
    def subClassOrSameTypeAs(other: Type) = ???
  }

  object Any extends Class("Any", Struct())
  object Nothing extends Class("Nothing", Struct())

  class SubClass(name: String, struct : Struct = Struct(), val parent: Class = Any) extends Class(name, struct)

  val numberClass = new SubClass("Number",
			      Struct(Map(
				"+" -> Some(functionClass),
				"-" -> Some(functionClass),
				"==" -> Some(functionClass))
			      )
			    )
  val booleanClass = new SubClass("Boolean", Struct(Map("==" -> Some(functionClass))))
  val functionClass = new SubClass("Function")

  class Method(val this_ : Object_, params: List[Param], body: Block) extends Function(body, Param("this") :: params) {
    override def apply_(arguments: List[Object_]) = super.apply_(this_ :: arguments)
  }

  case class AdditionExpression(ref1: Reference, ref2: Reference) extends Expression {
    def evaluate(assignements: Map[String, TypeMap], expected:Type) =
      (ref1.evaluate(assignements, expected), ref2.evaluate(assignements, expected)) match {
	case (Number_(x), Number_(y)) => Number_(x + y)
	case _ => throw new Error
      }
  }

  case class SubstractionExpression(ref1: Reference, ref2: Reference) extends Expression {
    def evaluate(assignements: Map[String, TypeMap], expected:Type) =
      (ref1.evaluate(assignements, expected), ref2.evaluate(assignements, expected)) match {
	case (Number_(x), Number_(y)) => Number_(x - y)
	case _ => throw new Error
      }
  }

  case class EqualityExpression(ref1: Reference, ref2: Reference) extends Expression {
    def evaluate(assignements: Map[String, TypeMap], expected:Type) =
      Boolean_(ref1.evaluate(assignements, expected) == ref2.evaluate(assignements, expected))
  }
  
  case class IfExpression(condition: Expression, body: Block, else_ : Option[Block] = None) extends Expression {
    def evaluate(assignements: Map[String, TypeMap], expected:Type) =
      condition.evaluate(assignements, expected) match {
	case Boolean_(true) => body.evaluate(assignements, expected)
	case _ => else_ match {
	  case Some(elseExpr) => elseExpr.evaluate(assignements, expected)
	  case None => nothing
	}
      }
  }

  case class Number_(value: Double) extends Object_(numberClass)
  {
    override def members = Map("+" ->  new Method(this,
			      List(Param("other", Some(numberClass.name))),
			      Block(AdditionExpression(Reference("this"), Reference("other")))),
		      "-" ->  new Method(this,
			      List(Param("other", Some(numberClass.name))),
			      Block(SubstractionExpression(Reference("this"), Reference("other")))),
		      "==" -> new Method(this,
			      List(Param("other", Some(Any.name))),
			      Block(EqualityExpression(Reference("this"), Reference("other")))))
		      
    override def toString = if (value.isValidInt) value.toInt.toString else value.toString
  }

  case class Boolean_(value: Boolean) extends Object_(booleanClass)
  {
    override def members = Map("==" -> new Method(this,
			      List(Param("other", Some(Any.name))),
			      Block(EqualityExpression(Reference("this"), Reference("other")))))
    override def toString = value.toString
  }
  
  object nothing extends Object_(Nothing) {
    override def toString = "nothing"
  }
}

class UnexpectedEndOfLineError extends Exception
import scala.util.parsing.combinator._

object Parsers extends RegexParsers with PackratParsers {
  import LanguageAST._
  import LanguageAST.ExpressionImplicits._

  val integer = """[1-9][0-9]*"""r
  
  val identifier = """[a-zA-Z+*=\-~<>]([a-zA-Z0-9+*=\-~<>]|_[a-zA-Z0-9])*"""r

  def reel = integer ~ "." ~ ("""[0-9][0-9]*"""r) ^^ {
    case (firstpart ~ "." ~ lastPart) => firstpart + "." + lastPart
  }

  def statement = (assignement | expression) ^^ { StatementCodeLine(_) }
  
  def codeLine = beginIfMultiLine | beginElseMultiLine | statement
  
  def beginIfMultiLine = "if" ~> "(" ~> expression <~ ")" <~ ":" ^^ { BeginIfMultiLine(_) }
  
  def beginElseMultiLine = ("else" ~> ":") ^^ { string => new BeginElseMultiLine }
  
  def assignement = reference ~ ":=" ~ expression ^^ {
    case (ref ~ ":=" ~ expression) => Assignement(ref, expression)
  }
  
  lazy val reference: PackratParser[Reference] = expression ~ "." ~ identifier ~ (":" ~> identifier).? ^^ {
    case (expression ~ "." ~ name ~ None) => Reference(name, None, Some(expression))
    case (expression ~ "." ~ name ~ Some(type_)) => Reference(name, Some(type_), Some(expression))
  } | identifier ^^ {
    case (name) => Reference(name)
  }
  
  lazy val expression: PackratParser[Expression] = ifExpression |
						   function |
						   function_call |
						   number |
						   boolean_ |
						   reference
						   
  def function = (("(" ~> repsep(identifier, ",") <~ ")" <~ "=>") ~ expression) ^^ {
    case (params ~ expression) => ObjectExpression(new Function(Block(expression), params.map(Param(_))))
  }
  
  def ifExpression = ("if" ~> "(" ~> expression <~ ")") ~ expression ~ ("else" ~> expression).? ^^ {
    case (condition ~ passExpression ~ None) => IfExpression(condition, Block(passExpression))
    case (condition ~ passExpression ~ Some(failExpresson)) => IfExpression(condition, Block(passExpression), Some(Block(failExpresson)))
  }

  def boolean_ = "true" ^^ { case _ => ObjectExpression(Boolean_(true)) } |
		"false" ^^ { case _ => ObjectExpression(Boolean_(false)) }
  
  def number = (reel | integer) ^^ (n => ObjectExpression(Number_(n.toDouble)))
  
  lazy val function_call: PackratParser[Application] = reference ~ ("(" ~> repsep(expression, ",") <~ ")") ^^ {
    case (reference ~ params) => Application(reference, params)
  } | expression ~ " " ~ identifier ~ " " ~ expression ^^ {
    case (expression ~ " " ~ identifier ~ " " ~ param) =>
    Application(Reference(identifier, None, Some(expression)), List(param))
  }
  
  trait CodeLine
  case class StatementCodeLine(statement: Statement) extends CodeLine
  case class BeginIfMultiLine(condition: Expression) extends CodeLine
  class BeginElseMultiLine extends CodeLine
}

object Parser {
  import LanguageAST._
  
  def parse(content: String): Block = {
    def getBlockAndRest(lines: List[String]) = {
      val (innerBlock, rest) = lines.span(_.startsWith("\t"))
      if (innerBlock.isEmpty) throw new Error
      else (innerBlock.map(_.tail), rest)
    }
    def parse(lines: List[String], statements: List[Statement]): Block =
      if (lines.isEmpty) Block(statements)
      else
	if (lines.head.startsWith("\t")) throw new Error
	else parseSingleCodeLine(lines.head) match {
	  case Parsers.Success(Parsers.BeginIfMultiLine(cond), _) => {
	    val (innerBlock, rest) = getBlockAndRest(lines.tail)
	    parse(rest, statements :+ IfExpression(cond, parse(innerBlock, Nil)))
	  }
	  case Parsers.Success(_ : Parsers.BeginElseMultiLine, _) => {
	    statements.last match {
	      case IfExpression(cond, ifBlock, None) => {
		val (innerBlock, rest) = getBlockAndRest(lines.tail)
		parse(rest, statements.init :+ IfExpression(cond, ifBlock, Some(parse(innerBlock, Nil))))
	      }
	      case _ => throw new Error
	    }
	  }
	  case Parsers.Success(Parsers.StatementCodeLine(statement), _) =>
	    parse(lines.tail, statements :+ statement)
	  case Parsers.Failure(_, _) =>
	    parse(lines.tail, statements :+ UncompiledStatement(lines.head))
	}
    parse(content.lines.toList, Nil)
  }
  
  def parseSingleCodeLine(content: String) = {
    if (content.contains('\n')) throw new UnexpectedEndOfLineError
    Parsers.parseAll(Parsers.codeLine, content)
  }
  
  def parseSingleValidCodeLine(content: String) = {
    parseSingleCodeLine(content) match {
      case Parsers.Success(stat, _) => stat
    }
  }
  
  def parseSingleValidStatement(content: String) =
    parseSingleValidCodeLine(content) match {
      case Parsers.StatementCodeLine(statement) => statement
    }
}

object WorkSheet {
  import LanguageAST._
  def computeResults(code: String): List[String] = {
    Parser.parse(code).evaluateOutput(Map(), Any)
  }
}