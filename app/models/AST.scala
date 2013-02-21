package models

object LanguageAST {

  trait Statement

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

  // TODO : implement this
  /*case class UncompiledExpression(content: String) extends Expression {
    def evaluate(assignements: Map[String, TypeMap], expected:Type) =
      Parser.parseSingleValidStatement(content).asInstanceOf[Expression].evaluate(assignements)
  }*/

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

  class Function(val params: List[Param], val body: Body) extends Object_(functionClass) {
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
  }

  case class Param(name: String, type_ : Option[String] = None)

  case class Body(content: List[Statement], expr: Expression) {
    def evaluate(assignements: Map[String, TypeMap]) = {
      var bodyAssignements = assignements
    
      for (statement <- content) {
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

  class Method(val this_ : Object_, params: List[Param], body: Body) extends Function(Param("this") :: params, body) {
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

  case class Number_(value: Double) extends Object_(numberClass)
  {
    override def members = Map("+" ->  new Method(this,
			      List(Param("other", Some(numberClass.name))),
			      Body(Nil, AdditionExpression(Reference("this"), Reference("other")))),
		      "-" ->  new Method(this,
			      List(Param("other", Some(numberClass.name))),
			      Body(Nil, SubstractionExpression(Reference("this"), Reference("other")))),
		      "==" -> new Method(this,
			      List(Param("other", Some(Any.name))),
			      Body(Nil, EqualityExpression(Reference("this"), Reference("other")))))
		      
    override def toString = if (value.isValidInt) value.toInt.toString else value.toString
  }

  case class Boolean_(value: Boolean) extends Object_(booleanClass)
  {
    override def members = Map("==" -> new Method(this,
			      List(Param("other", Some(Any.name))),
			      Body(Nil, EqualityExpression(Reference("this"), Reference("other")))))
    override def toString = value.toString
  }
}

class UnexpectedEndOfLineError extends Exception
import scala.util.parsing.combinator._

object StatementParser extends RegexParsers with PackratParsers {
  import LanguageAST._

  val integer = """[1-9][0-9]*"""r
  
  val identifier = """[a-zA-Z+*=\-]([a-zA-Z0-9+*=\-]|_[a-zA-Z0-9])*"""r

  def reel = integer ~ "." ~ ("""[0-9][0-9]*"""r) ^^ {
    case (firstpart ~ "." ~ lastPart) => firstpart + "." + lastPart
  }

  def statement = assignement | expression
  
  def assignement = reference ~ ":=" ~ expression ^^ {
    case (ref ~ ":=" ~ expression) => Assignement(ref, expression)
  }
  
  lazy val reference: PackratParser[Reference] = expression ~ "." ~ identifier ~ (":" ~ identifier).? ^^ {
    case (expression ~ "." ~ name ~ None) => Reference(name, None, Some(expression))
    case (expression ~ "." ~ name ~ Some(":" ~ type_)) => Reference(name, Some(type_), Some(expression))
  } | identifier ^^ {
    case (name) => Reference(name)
  }
  
  lazy val expression: PackratParser[Expression] = function_call |
						   number |
						   boolean_ |
						   reference

  def boolean_ = "true" ^^ { case _ => ObjectExpression(Boolean_(true)) } |
		"false" ^^ { case _ => ObjectExpression(Boolean_(false)) }
  
  def number = (reel | integer) ^^ (n => ObjectExpression(Number_(n.toDouble)))
  
  lazy val function_call: PackratParser[Application] = reference ~ ("(" ~> repsep(expression, ",") <~ ")") ^^ {
    case (reference ~ params) => Application(reference, params)
  } | expression ~ " " ~ identifier ~ " " ~ expression ^^ {
    case (expression ~ " " ~ identifier ~ " " ~ param) =>
    Application(Reference(identifier, None, Some(expression)), List(param))
  }
}

object Parser {
  import LanguageAST._
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
  import LanguageAST._
  def computeResults(code: String): List[String] = {
  
    var assignements: Map[String, TypeMap] = Map()
  
    code.split('\n').toList.map{ line =>
      if (line == "") ""
      else Parser.parseSingleStatement(line) match {
	case StatementParser.Success(ass: Assignement, _) => {
	  val evaluatedExpr = ass.value.evaluate(assignements)
	  ass.ref match {
	    case Reference(name, type_, None) => {
	      // TODO: Take expected type into account
	      val evaluatedExpr = ass.value.evaluate(assignements, Any)
	      assignements = assignements + (name -> TypeMap(Map(type_ -> evaluatedExpr)))
	      name + " = " + evaluatedExpr.toString
	    }
	    case Reference(name, type_, Some(expr)) => {
	      // if is assignable, modify variable in assignements to reflect this assignement
	      "Currently unsupported assignement"
	    }
	  }
	}
	case StatementParser.Success(ref: Reference, _) => ref.evaluate(assignements) match {
	  case resultRef: Reference if (resultRef == ref) => s"invalid reference: ${ref.name}"
	  case value => value.toString
	}
	case StatementParser.Success(exp: Expression, _) => exp.evaluate(assignements) match {
	  case obj: Object_ => obj.toString
	  case _ => "invalid expression"
	}
	case _ => "invalid statement"
      }
    }
  }
}