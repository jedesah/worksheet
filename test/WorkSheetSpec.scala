package test

import org.specs2.mutable._

import models.AST._

class WorkSheetSpec extends Specification {

  "WorkSheet" should {

    "compute a simple numerical expression" in {
      val code = "+(7, 4)"
      
      WorkSheet.computeResults(code) must be equalTo List("11")
    }
  }
}