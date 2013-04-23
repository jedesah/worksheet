package models

import models.Edits._
import models.Edits.Insert
import models.Edits.TextPosition
import play.api.libs.iteratee.Concurrent
import play.api.libs.json.Json

object WorkSpaces {
  private var impl: List[WorkSpace] = Nil
  def create: WorkSpace = {
    val newWorkSpace = new WorkSpace(impl.size.toString)
    impl = newWorkSpace :: impl
    newWorkSpace
  }
  def get(id: String): WorkSpace = impl.find(_.id == id).get
}

class WorkSpace(val id: String) {
  val (enumerator, channel) = Concurrent.broadcast[String]
  var code: TextEdit = NilEdit
  var codeSheet: CodeSheet = ScalaCodeSheet

  implicit val userWrites = Json.writes[CodeChangeResult]

  def modifyCode(newCode: String) = {
    println("allo")
    println(newCode)
    val rpc = Json.parse(newCode)
    println("alloa")
    val language = (rpc \ "evaluationLanguage").as[String]
    println("allob")
    val change = rpc \ "editorDelta"
    println("alloc")
    val action = (change \ "action").as[String]
    println("allo1")
    val row = (change \ "range" \ "start" \ "row").as[Int]
    println("allo2")
    val column = (change \ "range" \ "start" \ "column").as[Int]
    println("allo3")
    val text = (change \ "text").as[String]
    println("allo4")
    action match {
      case "insertText" => code = Insert(TextPosition(row, column), text, code)
      case "removeText" => code = Delete(TextPosition(row, column + text.size), text.size, code)
      case _ => println(action)
    }
    println("allo5")
    println(code.textValue.asString)
    codeSheet = language match {
      case "Scala" => ScalaCodeSheet
      case "JR" => JrCodeSheet
    }
    channel.push(Json.stringify(Json.toJson(CodeChangeResult(newCode, evaluateCode))))
  }

  def evaluateCode = codeSheet.computeResults(code.textValue.asString)

}

case class CodeChangeResult(code: String, evaluatedOutput: List[String])