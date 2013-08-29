package models

import models.Edits._
import models.Edits.Insert
import models.Edits.TextPosition
import play.api.libs.iteratee.Concurrent
import play.api.libs.json.Json

object WorkSpaces {
  private var impl: List[WorkSpace] = Nil
  def create(initialLanguage: CodeSheet, init: String = ""): WorkSpace = {
    val newWorkSpace = new WorkSpace(impl.size.toString, initialLanguage, init)
    impl = newWorkSpace :: impl
    newWorkSpace
  }
  def get(id: String): WorkSpace = impl.find(_.id == id).get
}

class WorkSpace(val id: String, initialLanguage: CodeSheet, init: String = "") {
  val (enumerator, channel) = Concurrent.broadcast[String]
  var code: TextEdit = BaseEdit(init)
  var codeSheet: CodeSheet = initialLanguage

  implicit val userWrites = Json.writes[CodeChangeResult]

  def modifyCode(newCode: String) = {
    val rpc = Json.parse(newCode)
    val language = (rpc \ "evaluationLanguage").as[String]
    val change = rpc \ "editorDelta"
    val action = (change \ "action").as[String]
    val row = (change \ "range" \ "start" \ "row").as[Int]
    val column = (change \ "range" \ "start" \ "column").as[Int]
    val text = (change \ "text").as[String]
    action match {
      case "insertText" => code = Insert(TextPosition(row, column), text, code)
      case "removeText" => code = Delete(TextPosition(row, column + text.size), text.size, code)
      case _ => println(action)
    }
    codeSheet = language match {
      //case "Scala" => ScalaCodeSheet
      case "JR" => JrCodeSheet
    }
    channel.push(Json.stringify(Json.toJson(CodeChangeResult(newCode, evaluateCode))))
  }

  def evaluateCode = codeSheet.computeResults(code.textValue.asString).mkString("\n")

}

case class CodeChangeResult(code: String, evaluatedOutput: String)