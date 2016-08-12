package org

import java.nio.charset.StandardCharsets
import java.util.Base64

import org.ActorStore.{NXProfile, NXActor}
import org.StormPathAuthenticationService.StormPathConfig
import play.api.Logger
import play.api.libs.json.{JsValue, Json}
import play.api.libs.ws.{WSAuthScheme, WSClient}
import triplestore.GraphProperties._
import triplestore.Sparql._
import triplestore.TripleStore

import scala.concurrent.{ExecutionContext, Future}

trait AuthenticationService {

  def oAuthenticated(actorName: String): Future[NXActor]

  def authenticate(actorName: String, password: String): Future[Option[NXActor]]

}

class MockAuthenticationService extends AuthenticationService {

  val adminUser = NXActor("admin", None, Some(NXProfile("admin", "admin", "admin@localdev.org")))

  override def oAuthenticated(actorName: String) = Future.successful(adminUser)
  override def authenticate(actorName: String, password: String) = Future.successful(Some(adminUser))
}

class TsBasedAuthenticationService()(implicit ec: ExecutionContext, ts: TripleStore) extends AuthenticationService {

  def oAuthenticated(actorName: String): Future[NXActor] = {
    ts.query(getActor(actorName)).map(actorFromResult).flatMap { actorOpt =>
      actorOpt.map(Future.successful).getOrElse {
        val newActor = NXActor(actorName, None)
        ts.up.sparqlUpdate(insertOAuthActorQ(newActor)).map(ok => newActor)
      }
    }
  }

  def authenticate(actorName: String, password: String): Future[Option[NXActor]] = {
    val passwordHashString = Utils.hashPasswordUnsecure(password, actorName)
    ts.query(getActorWithPassword(actorName, passwordHashString)).map(actorFromResult).flatMap { actorOpt =>
      if (actorOpt.isEmpty) {
        ts.ask(graphExistsQ(actorsGraph)).flatMap { exists =>
          if (exists) {
            Future.successful(None)
          }
          else {
            val topActor = NXActor(actorName, None)
            ts.up.sparqlUpdate(insertTopActorQ(topActor, passwordHashString)).map(ok => Some(topActor))
          }
        }
      }
      else {
        Future.successful(actorOpt)
      }
    }
  }
}

object StormPathAuthenticationService {

  val STORMPATH_BASE_URL = "https://api.stormpath.com/v1/"
  case class StormPathConfig(appId: String, apiKey: String, apiSecret: String)

}

class StormPathAuthenticationService(val wsClient: WSClient, val config: StormPathConfig)(implicit ec: ExecutionContext) extends AuthenticationService {

  case class AuthenticateResponse(href: String)
  case class ErrorResponse(status: Int, code: Int, message: String, developerMessage: String, moreInfo: String)
  case class AccountDetailsResponse(foo: String)

  import StormPathAuthenticationService.STORMPATH_BASE_URL

  def loginEndpoint(appId: String) = s"${STORMPATH_BASE_URL}applications/$appId/loginAttempts"

  override def oAuthenticated(actorName: String) = ???

  private def base64Of(uid: String, password: String): String = {
    val concatenated = s"uid:password".getBytes(StandardCharsets.UTF_8)
    Base64.getEncoder.encodeToString(concatenated)
  }

  private def getAccount(authResult: Either[AuthenticateResponse, ErrorResponse]): Future[Option[AccountDetailsResponse]] = {
    authResult match {
      case Right(failed) => Future.successful(None)
      case Left(success) => {
        wsClient.url(success.href).
          withAuth(config.apiKey, config.apiSecret, WSAuthScheme.BASIC).
          get().
          map(detailsJson => {
            Logger.info(s"json: $detailsJson")
            Some(AccountDetailsResponse("bar"))
          })
      }
    }
  }

  private def doAuthenticateCall(actorName: String, password: String): Future[Either[AuthenticateResponse, ErrorResponse]] = {
    val json = Json.obj(
      "type" -> "basic",
      "value" -> base64Of(actorName, password)
    )
    wsClient.url(loginEndpoint(config.appId)).
      withAuth(config.apiKey, config.apiSecret, WSAuthScheme.BASIC).
      post(json).
      map {response =>
        Logger.debug(s"response: ${response.json}")
        parseAuthenticateResponse(response.json)
      }
  }

  private def parseAuthenticateResponse(json: JsValue): Either[AuthenticateResponse, ErrorResponse] = {
    Logger.info(s"received ${json}")
    // see if there is an error, if there isn't just try to parse the account
    val maybeValue: Option[JsValue] = (json \ "account").toOption
    if (!maybeValue.isDefined) {
      Right(parseStormpathError(json))
    } else {
      val href = (json \ "account" \ "href").get
      Left(new AuthenticateResponse(href.toString))
    }
  }

  private def parseStormpathError(json: JsValue): ErrorResponse = {
    ErrorResponse((json \ "status").as[Int],
      (json \ "code").as[Int],
      (json \ "message").get.toString(),
      (json \ "developerMessage").get.toString(),
      (json \ "moreInfo").get.toString()
    )
  }
  override def authenticate(actorName: String, password: String) = {
    doAuthenticateCall(actorName, password).
      flatMap { eitherResponseOrError =>
        getAccount(eitherResponseOrError)
      }.map { accountDetailsOpt =>
        accountDetailsOpt match {
          case None => None
          case Some(accountDetails) => Some(NXActor(accountDetails.foo, None, None))
        }
    }
  }
}
