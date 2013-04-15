package controllers

import play.api._
import play.api.mvc._

import play.api.libs.iteratee._
import play.api.libs.json._

import models.WorkSheet
import scala.tools.nsc.interpreter.IMain
import models.Edits._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import scala.tools.reflect.ToolBoxError

object Application extends Controller {
  def index = Action { implicit request => 
    Ok(views.html.index(default.code.textValue.asString))
  }

  def index1 = Action { implicit request =>
    Ok(views.html.index1())
  }
  
  val default = new WorkSpace
  
  def update = WebSocket.using[String] { implicit request =>
    
    val in = Iteratee.foreach[String](content => {
      default.modifyCode(content)
    })
    
    (in, default.enumerator)
  }
  
	val cm = ru.runtimeMirror(getClass.getClassLoader)
	val toolBox = cm.mkToolBox()
  
	class WorkSpace {
		val (enumerator, channel) = Concurrent.broadcast[String]
		var code: TextEdit = NilEdit
		
		implicit val userWrites = Json.writes[CodeChangeResult]
	
		def modifyCode(newCode: String) = {
      println("allo")
      val change = Json.parse(newCode)
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
			channel.push(Json.stringify(Json.toJson(CodeChangeResult(newCode, ScalaWorkSheet.computeResults(code.textValue.asString).mkString("\n")))))
		}
	}
	
	case class CodeChangeResult(code: String, evaluatedOutput: String)
	
	object ScalaWorkSheet {
		def computeResults(code: String): List[String] = {
			var accu = ""
			code.lines.toList.map{ line =>
			  
			  try {
			    
			    val subTree = toolBox.parse(line)
			    println(showRaw(subTree))
			    val oldAccu = accu
			    accu = accu + "\n" + line
			    subTree match {
			      case ValDef(_, newTermName, _, expr) =>
				newTermName + " = " + toolBox.eval(toolBox.parse(oldAccu + "\n" + expr.toString)).toString
			      case _            => toolBox.eval(toolBox.parse(accu)).toString
			    } 
			  } catch {  
			    case ToolBoxError(msg, cause) => {
			      msg.dropWhile(_ != ':').drop(2)
			    }
			  }
			}
		}
	}
  
  def driveTest = Action { request =>
    import com.google.api.client.googleapis.auth.oauth2.GoogleAuthorizationCodeFlow
    import com.google.api.client.googleapis.auth.oauth2.GoogleCredential
    import com.google.api.client.googleapis.auth.oauth2.GoogleTokenResponse
    import com.google.api.client.http.FileContent
    import com.google.api.client.http.HttpTransport
    import com.google.api.client.http.javanet.NetHttpTransport
    import com.google.api.client.json.JsonFactory
    import com.google.api.client.json.jackson.JacksonFactory
    import com.google.api.services.drive.Drive
    import com.google.api.services.drive.DriveScopes
    import com.google.api.services.drive.model.File

    import java.io.BufferedReader
    import java.io.IOException
    import java.io.InputStreamReader
    import java.util.Arrays


    val CLIENT_ID = "YOUR_CLIENT_ID"
    val CLIENT_SECRET = "YOUR_CLIENT_SECRET"

    val REDIRECT_URI = "urn:ietf:wg:oauth:2.0:oob"
  
    val httpTransport = new NetHttpTransport()
    val jsonFactory = new JacksonFactory()
   
    val flow = new GoogleAuthorizationCodeFlow.Builder(
        httpTransport, jsonFactory, CLIENT_ID, CLIENT_SECRET, Arrays.asList(DriveScopes.DRIVE))
        .setAccessType("online")
        .setApprovalPrompt("auto").build()
    
    val url = flow.newAuthorizationUrl().setRedirectUri(REDIRECT_URI).build()

    //Insert a file  
    val body = new File()
    body.setTitle("My document")
    body.setDescription("A test document")
    body.setMimeType("text/plain")
    
    val fileContent = new java.io.File("document.txt")
    val mediaContent = new FileContent("text/plain", fileContent)

    //val file = service.files().insert(body, mediaContent).execute()
    Ok("driveTest")
  }
}