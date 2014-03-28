package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json._

/** Application controller, handles authentication */
object Application extends Controller with Security {

  /** Serves the index page, see views/index.scala.html */
  def index = Action {
    Ok(views.html.index())
  }

  /**
   * Returns the JavaScript router that the client can use for "type-safe" routes.
   * @param varName The name of the global variable, defaults to `jsRoutes`
   */
  def jsRoutes(varName: String = "jsRoutes") = Action {
    implicit request =>
      Ok(
        Routes.javascriptRouter(varName)(
          routes.javascript.Application.login,
          routes.javascript.Application.logout,
          routes.javascript.Users.user,
          routes.javascript.Users.createUser,
          routes.javascript.Users.updateUser,
          routes.javascript.Users.deleteUser,
          routes.javascript.FileHandling.list
        )
      ).as(JAVASCRIPT)
  }

  /**
   * Log-in a user. Pass the credentials as JSON body.
   * @return The token needed for subsequent requests
   */
  def login() = Action(parse.json) {
    implicit request =>
      val token = java.util.UUID.randomUUID().toString
      val email = (request.body \ "email").as[String]
      // todo: use password
      Ok(Json.obj(
        "token" -> token,
        "user" -> Json.obj("email" -> email)
      )).withToken(token -> email)
  }

  /** Logs the user out, i.e. invalidated the token. */
  def logout() = HasToken(parse.json) {
    token => email => implicit request =>
    // TODO Invalidate token, remove cookie
    Ok.discardingToken(token)
  }

}
