package controllers

import play.api._
import play.api.mvc._

object Application extends Controller {
  
  def index = Action {
    Ok(views.html.index())
  }
  
  def eval = Action { request =>
    println(request.body.asFormUrlEncoded.get("code"))
    Ok(views.html.index())
  }
  
  /*def update = WebSocket.using[String]*/
  
}