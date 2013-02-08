package test

import org.specs2.mutable._

import models.AST._

class WorkSheetSpec extends Specification {

  "WorkSheet" should {

    "compute a simple numerical expression such as \"7.+(4)\"" in {
      val code = "7.+(4)"
      
      WorkSheet.computeResults(code) must be equalTo List("11")
    }
    
    "handle an incorrect expression such as \"7.\" by printing \"invalid expression\"" in {
      val code = "7."
      
      WorkSheet.computeResults(code) must be equalTo List("invalid statement")
    }
    
    "handle an invalid reference by printing \"invalid reference: a\" for a reference to an unknown a" in {
      val code = "a"
      
      WorkSheet.computeResults(code) must be equalTo List("invalid reference: a")
    }
    
    "handle an unresolved expression assignement such as \"a := b.+(c)\"" in {
      val code = "a := b.+(c)"
      
      WorkSheet.computeResults(code) must be equalTo List("a = b.+(c)")
    }
    
    "handle a simple assignement statement such as \"a := 10\"" in {
      val code = "a := 10"
      
      WorkSheet.computeResults(code) must be equalTo List("a = 10")
    }
    
    val example_resolve_unresolved = """a := b.+(c)
b := 10
c := 5
a"""
    s"handle resolving an unresolved expression such as $example_resolve_unresolved" in {
      val expected = List("a = b.+(c)", "b = 10", "c = 5", "15")
      WorkSheet.computeResults(example_resolve_unresolved) must be equalTo expected
    }
  }
}