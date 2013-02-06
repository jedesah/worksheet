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
      
      WorkSheet.computeResults(code) must be equalTo List("invalid expression")
    }
    
    "handle an invalid reference by printing \"invalid reference: a\" for a reference to an unknown a" in {
      val code = "a"
      
      WorkSheet.computeResults(code) must be equalTo List("invalid reference: a")
    }
  }
}