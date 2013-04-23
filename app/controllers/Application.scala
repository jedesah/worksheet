package controllers

import play.api._
import play.api.mvc._

import play.api.libs.iteratee._
import play.api.libs.json._

import models._
import scala.tools.nsc.interpreter.IMain
import models.Edits._

object Application extends Controller {
  def home_page = Action { implicit request =>
    Ok(views.html.home_page())
  }

  def scalaCodeSheet = Action { implicit request =>
    Ok(views.html.code_sheet(defaultScalaWorkSpace, "Scala"))
  }

  def jrCodeSheet = Action { implicit request =>
    Ok(views.html.code_sheet(defaultJRWorkSpace, "JR"))
  }

  val defaultScalaWorkSpace = WorkSpaces.create
  val defaultJRWorkSpace = WorkSpaces.create
  
  def update(id: String) = WebSocket.using[String] { implicit request =>

    val workSpace = WorkSpaces.get(id)
    
    val in = Iteratee.foreach[String](content => {
      workSpace.modifyCode(content)
    })
    
    (in, workSpace.enumerator)
  }
  
  /*def driveTest = Action { request =>
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
  }*/
}