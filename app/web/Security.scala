//===========================================================================
//    Copyright 2014 Delving B.V.
//
//    Licensed under the Apache License, Version 2.0 (the "License");
//    you may not use this file except in compliance with the License.
//    You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
//    Unless required by applicable law or agreed to in writing, software
//    distributed under the License is distributed on an "AS IS" BASIS,
//    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//    See the License for the specific language governing permissions and
//    limitations under the License.
//===========================================================================

package web

import play.Logger
import play.api.Play.current
import play.api.cache.Cache
import play.api.libs.json._
import play.api.mvc._

import scala.concurrent.Future

trait Security {
  this: Controller =>
  val TOKEN = "X-XSRF-TOKEN"
  val TOKEN_COOKIE_KEY = "XSRF-TOKEN"
  lazy val CACHE_EXPIRATION = play.api.Play.current.configuration.getInt("cache.expiration").getOrElse(60 * 60 * 4)

  case class CachedProfile
  (
    firstName: String, lastName: String,
    email:String,
    apiKey: String,
    narthexDomain:String, naveDomain:String,
    categoriesEnabled: Boolean
    )

  /*
    To make this work seamlessly with Angular, you should read the token from a header called
    X-XSRF-TOKEN, and issue a cookie called XSRF-TOKEN.
    Check out my blog post that explains this in detail:
    http://www.mariussoutier.com/blog/2013/07/14/272/
  */

  def Secure[A](p: BodyParser[A] = parse.anyContent)(block: CachedProfile => Request[A] => Result): Action[A] = Action(p) { implicit request =>
    val maybeToken: Option[String] = request.headers.get(TOKEN)
    val maybeCookie: Option[String] = request.cookies.get(TOKEN_COOKIE_KEY).map(_.value)
    val tokenOrCookie: Option[String] = if (maybeToken.isDefined) maybeToken else maybeCookie
    tokenOrCookie.flatMap { token =>
      Cache.getAs[CachedProfile](token) map { cachedProfile =>
        block(cachedProfile)(request).withToken(token, cachedProfile)
      }
    } getOrElse {
      Unauthorized(Json.obj("err" -> "Secure session expired"))
    }
  }

  def SecureAsync[A](p: BodyParser[A] = parse.anyContent)(block: CachedProfile => Request[A] => Future[Result]): Action[A] = Action.async(p) { implicit request =>
    val maybeToken: Option[String] = request.headers.get(TOKEN)
    val maybeCookie: Option[String] = request.cookies.get(TOKEN_COOKIE_KEY).map(_.value)
    val tokenOrCookie: Option[String] = if (maybeToken.isDefined) maybeToken else maybeCookie
    tokenOrCookie.flatMap { token =>
      Cache.getAs[CachedProfile](token) map { cachedProfile =>
        block(cachedProfile)(request)
      }
    } getOrElse {
      Future.successful(Unauthorized(Json.obj("err" -> "Secure async session expired")))
    }
  }

  implicit class ResultWithToken(result: Result) {
    def withToken(token: String, cachedProfile: CachedProfile): Result = {
      Cache.set(token, cachedProfile, CACHE_EXPIRATION)
      result.withCookies(Cookie(TOKEN_COOKIE_KEY, token, None, httpOnly = false))
    }

    def discardingToken(token: String): Result = {
      Logger.info(s"discarding token $token")
      Cache.remove(token)
      result.discardingCookies(DiscardingCookie(name = TOKEN_COOKIE_KEY))
    }
  }

}
