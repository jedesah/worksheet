package test

import org.specs2.mutable._

import models.JrCodeSheet

class WorkSheetSpec extends Specification {

  "WorkSheet" should {

    "compute a simple numerical expression such as \"7.+(4)\"" in {
      val code = "7.+(4)"

      JrCodeSheet.computeResults(code) must be equalTo List("11")
    }
    
    "handle an incorrect expression such as \"7.\" by printing \"invalid expression\"" in {
      val code = "7."

      JrCodeSheet.computeResults(code) must be equalTo List("invalid statement")
    }
    
    "handle an invalid reference by printing \"invalid reference: a\" for a reference to an unknown a" in {
      val code = "a"

      JrCodeSheet.computeResults(code) must be equalTo List("invalid reference: a")
    }
    
    "handle an unresolved expression assignement such as \"a := b.+(c)\"" in {
      val code = "a := b.+(c)"

      JrCodeSheet.computeResults(code) must be equalTo List("a = b.+(c)")
    }
    
    "handle a simple assignement statement such as \"a := 10\"" in {
      val code = "a := 10"

      JrCodeSheet.computeResults(code) must be equalTo List("a = 10")
    }
    
    val example_resolve_unresolved = """a := b.+(c)
b := 10
c := 5
a"""
    s"handle resolving an unresolved expression such as $example_resolve_unresolved" in {
      val expected = List("a = b.+(c)", "b = 10", "c = 5", "15")
      JrCodeSheet.computeResults(example_resolve_unresolved) must be equalTo expected
    }
    
    val example_unresolved = """a := 10
b := c.+(a)"""
    s"handle partially unresolved expression such as $example_unresolved" in {
      val expected = List("a = 10", "b = c.+(10)")
      JrCodeSheet.computeResults(example_unresolved) must be equalTo expected
    }
    
    val boolean_comparison = "true.==(false)"
    s"handle boolean comparison of the form $boolean_comparison" in {
      val expected = List("false")
      JrCodeSheet.computeResults(boolean_comparison) must be equalTo expected
    }
    
    val number_comparison = "1.==(1)"
    s"handle number comparison of the form $number_comparison" in {
      val expected = List("true")
      JrCodeSheet.computeResults(number_comparison) must be equalTo expected
    }
    
    val mixed_comparison = """a := 10
a.==(10)
10.==(a)"""
    s"handle a mixed comparison of the form $mixed_comparison" in {
      val expected = List("a = 10", "true", "true")
      JrCodeSheet.computeResults(mixed_comparison) must be equalTo expected
    }
    
    val simple_pass_if_expression = "if (true) 7"
    s"compute the result of a simple if expression of the form $simple_pass_if_expression" in {
      val expected = List("7")
      JrCodeSheet.computeResults(simple_pass_if_expression) must be equalTo expected
    }
    
    val simple_fail_if_expression = "if (false) 7"
    s"compute the result of a simple if expression of the form $simple_fail_if_expression" in {
      val expected = List("nothing")
      JrCodeSheet.computeResults(simple_fail_if_expression) must be equalTo expected
    }
    
    val simple_pass_if_else_expression = """a := 10
b := 10
if (a.==(b)) 5 else 10"""
    s"compute the result of a passing if/else expression of the form $simple_pass_if_else_expression" in {
      val expected = List("a = 10", "b = 10", "5")
      JrCodeSheet.computeResults(simple_pass_if_else_expression) must be equalTo expected
    }
    
    val simple_fail_if_else_expression = """a := 6
b := 5
if (a.==(b)) 5 else 10"""
    s"compute the resul of a failing if/else expression of the form $simple_fail_if_else_expression" in {
      val expected = List("a = 6", "b = 5", "10")
      JrCodeSheet.computeResults(simple_fail_if_else_expression) must be equalTo expected
    }
    
    val multiline_passing_if_expression = """if (true):
	7"""
    s"compute the result of a passing multiline if expression of the form $multiline_passing_if_expression" in {
      val expected = List("7")
      JrCodeSheet.computeResults(multiline_passing_if_expression) must be equalTo expected
    }
    
    val multiline_failing_if_expression = """if (false):
	7"""
    s"compute the result of a failing multiline if expression of the form $multiline_failing_if_expression" in {
      val expected = List("nothing")
      JrCodeSheet.computeResults(multiline_failing_if_expression) must be equalTo expected
    }
    
    val multiline_passing_if_else_expression = """if (true):
	7
else:
	3"""
    s"compute the result of a passing multiline if/else expression of the form $multiline_passing_if_else_expression" in {
      val expected = List("7")
      JrCodeSheet.computeResults(multiline_passing_if_else_expression) must be equalTo expected
    }
    
    val multiline_failing_if_else_expression = """if (false):
	7
else:
	3"""
    s"compute the result of a failing multiline if/else expression of the form $multiline_failing_if_else_expression" in {
      val expected = List("3")
      JrCodeSheet.computeResults(multiline_failing_if_else_expression) must be equalTo expected
    }
    
    val multiline_failing_if_else_expression_with_syntax_errors = """if (false):
	car.
else:
	3"""
    s"compute the result of a failing multiline if/else expression with syntax errors of the form $multiline_failing_if_else_expression_with_syntax_errors" in {
      val expected = List("3")
      JrCodeSheet.computeResults(multiline_failing_if_else_expression_with_syntax_errors) must be equalTo expected
    }
    
    val multiline_passing_if_else_expression_with_syntax_errors = """if (true):
	7
else:
	car."""
    s"compute the result of a passing multiline if/else expression with syntax errors of the form $multiline_passing_if_else_expression_with_syntax_errors" in {
      val expected = List("7")
      JrCodeSheet.computeResults(multiline_passing_if_else_expression_with_syntax_errors) must be equalTo expected
    }
    
    val equal_simple_assignation = "a = 10"
    s"handle simple expression assignation with equal resolver of the form $equal_simple_assignation" in {
      val expected = List("a = 10")
      JrCodeSheet.computeResults(equal_simple_assignation) must be equalTo expected
    }
    
    val simple_function_declaration = "bbe := () => 7"
    s"a simple function can be declared as per the following example: $simple_function_declaration" in {
      val expected = List("bbe = () =>")
      JrCodeSheet.computeResults(simple_function_declaration) must be equalTo expected
    }
    
    val declaring_and_using_a_function = """test := (a) => if(a) 7 else 10
test(true)
test(false)"""
	s"compute a function assignement followed by applications of that function" in {
		val expected = List("test = (a) =>", "7", "10")
    JrCodeSheet.computeResults(declaring_and_using_a_function) must be equalTo expected
	}
	
	val bug_1 = """michel_function := (a) => if (a.==(10)) 9 else 8
michel_function(0)"""
	"bug1" in {
		val expected = List("michel_function = (a) =>", "8")
    JrCodeSheet.computeResults(bug_1) must be equalTo expected
	}
    
    val optional_typing = """cars := getXMLOfCar()
car := Car.fromXML(car)
verifyXml(car)"""
  }
}