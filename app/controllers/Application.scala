package controllers

import play.api._
import play.api.mvc._

import models.AST._

object Application extends Controller {
  
  def index = Action { implicit request =>
    Ok(views.html.index())
  }
  
  def eval = Action { implicit request =>
    val code = request.body.asFormUrlEncoded.get("code").head

    val results = WorkSheet.computeResults(code)

    Ok(views.html.index(code, results))
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
}