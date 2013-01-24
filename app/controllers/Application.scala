package controllers

import play.api._
import play.api.mvc._

import models.AST._

object Application extends Controller {
  
  def index = Action {
    Ok(views.html.index())
  }
  
  def eval = Action { request =>
    val code = request.body.asFormUrlEncoded.get("code").head

    val results = WorkSheet.computeResults(code)

    Ok(views.html.index(code, results))
  }
  
  /*def update = WebSocket.using[String]*/
  
}