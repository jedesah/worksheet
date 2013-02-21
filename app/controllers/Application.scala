package controllers

import play.api._
import play.api.mvc._

import models.WorkSheet

object Application extends Controller {
  
  def index = Action { implicit request =>
    Ok(views.html.index())
  }
  
  def update = WebSocket.using[String] { implicit request =>
    import play.api.libs.iteratee._
    import play.api.libs.json._

    val (enumerator, channel) = Concurrent.broadcast[String]
    
    val in = Iteratee.foreach[String](content => {
      channel.push(Json.stringify(Json.toJson(WorkSheet.computeResults(content))))
    })
    
    (in, enumerator)
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