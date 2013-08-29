package controllers

import play.api.mvc._

import play.api.libs.iteratee._
import play.api.libs.iteratee.Concurrent

import scala.concurrent._
import ExecutionContext.Implicits.global

import models._
//import com.google.api.client.googleapis.auth.oauth2.{GoogleClientSecrets, GoogleAuthorizationCodeTokenRequest}

object Application extends Controller {
  def home_page = Action { implicit request =>
    Ok(views.html.home_page())
  }

  def credit = Action { implicit request =>
    Ok(views.html.credit())
  }

  def about = Action { implicit request =>
    Ok(views.html.about())
  }

  def scalaCodeSheet = Action { implicit request =>
    Ok(views.html.code_sheet(defaultScalaWorkSpace, "Scala"))
  }

  def jrCodeSheet = Action { implicit request =>
    Ok(views.html.code_sheet(defaultJRWorkSpace, "JR"))
  }

  def demo1_JRCodeSheet = Action { implicit request =>
    Ok(views.html.code_sheet(demo1JRWorkSpace, "JR"))
  }

  def demo1_ScalaCodeSheet = Action { implicit request =>
    Ok(views.html.code_sheet(demo1ScalaWorkSpace, "Scala"))
  }

  val demo1Code = """a := 10
b := e.+(c)
b
c := 5
e := 9
b
"""

  val scalaDemoCode =
    """val a = 10
      |val b = a + -6
      |val result = List(1,2,3,4).take(Math.sqrt(b).toInt)""".stripMargin

  val defaultScalaWorkSpace = WorkSpaces.create(JrCodeSheet)
  val defaultJRWorkSpace = WorkSpaces.create(JrCodeSheet)
  val demo1JRWorkSpace = WorkSpaces.create(JrCodeSheet, demo1Code)
  val demo1ScalaWorkSpace = WorkSpaces.create(JrCodeSheet, scalaDemoCode)
  
  def eval = WebSocket.using[String] { implicit request =>

    val (enumerator, channel) = Concurrent.broadcast[String]

    val in = Iteratee.foreach[String](content => {
      val result = future { com.github.jedesah.codesheet.api.ScalaCodeSheet.computeResults(content).mkString("\n") }
      result onSuccess {
        case result => channel.push(result)
      }
    })
    
    (in, enumerator)
  }

/*def driveTest = Action { request =>
    import com.google.api.client.googleapis.auth.oauth2.GoogleAuthorizationCodeFlow
    import com.google.api.client.auth.oauth2.Credential
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

    val transport = new NetHttpTransport()
    val json_factory = new JacksonFactory()
    val secrets = new GoogleClientSecrets()
    val CLIENT_ID = "888337082644.apps.googleusercontent.com"
    val CLIENT_SECRET = "Zw1gOpeT7rHO_Br0UQfTtJvJ"
    val REDIRECT_URI = "myURL"
    var secrets = new GoogleClientSecrets()
    secrets.set("client_id", CLIENT_ID)
    secrets.set("client_secret", CLIENT_SECRET)
    secrets.set("redirect_uris", )

    def exchangeCode(authorizationCode: String): Credential = {
       val response: GoogleTokenResponse = new GoogleAuthorizationCodeTokenRequest(
          transport,
          json_factory,
          CLIENT_ID,
          CLIENT_SECRET,
          authorizationCode,
          REDIRECT_URI).execute()
        val cred = new GoogleCredential.Builder()

      )
    }




  
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