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

import java.io.{File, FileInputStream, FileNotFoundException}
import java.util.UUID

import org.ActorStore.{NXActor, NXProfile}
import org.OrgContext._
import play.api.Play.current
import play.api._
import play.api.cache.Cache
import play.api.http.{HeaderNames, MimeTypes}
import play.api.libs.iteratee.Enumerator
import play.api.libs.json._
import play.api.libs.ws.WS
import play.api.mvc._
import play.api.routing.JavaScriptReverseRouter
import services.MailService.{MailDatasetError, MailProcessingComplete}

import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object MainController extends Controller with Security {

  val SIP_APP_VERSION = "1.0.9"

  val SIP_APP_URL = s"http://artifactory.delving.org/artifactory/delving/eu/delving/sip-app/$SIP_APP_VERSION/sip-app-$SIP_APP_VERSION-exejar.jar"

  def actorSession(actor: NXActor) = ActorSession(
    actor,
    apiKey = API_ACCESS_KEYS(0),
    narthexDomain = NARTHEX_DOMAIN,
    naveDomain = NAVE_DOMAIN
  )

  def root = Action { request =>
    Redirect("/narthex/")
  }

  def index = Action { request =>
    Ok(views.html.index(ORG_ID, SIP_APP_URL, buildinfo.BuildInfo.version))
  }

  def login = Action.async(parse.json) { implicit request =>
    val username = (request.body \ "username").as[String]
    val password = (request.body \ "password").as[String]
    Logger.info(s"Login $username")
    orgContext.authenticationService.authenticate(username, password).map { actorOpt =>
      actorOpt.map { actor =>
        val session = actorSession(actor)
        Ok(Json.toJson(session)).withSession(session)
      } getOrElse {
        Unauthorized("Username/password not found")
      }
    }
  }

  def checkLogin = Action { implicit request =>
    val maybeToken = request.headers.get(TOKEN)
    maybeToken flatMap {
      token =>
        Cache.getAs[ActorSession](token) map { session =>
          Ok(Json.toJson(session)).withSession(session)
        }
    } getOrElse Unauthorized(Json.obj("err" -> "Check login failed")).discardingToken(TOKEN)
  }

  /** Logs the user out, i.e. invalidated the token. */
  def logout = Action { implicit request =>
    Logger.info(s"Logout")
    request.headers.get(TOKEN) match {
      case Some(token) =>
        Ok.discardingToken(token)
      case None =>
        Unauthorized(Json.obj("err" -> "Logout failed"))
    }
  }

  def setProfile() = SecureAsync(parse.json) { session => implicit request =>
    val nxProfile = NXProfile(
      firstName = (request.body \ "firstName").as[String],
      lastName = (request.body \ "lastName").as[String],
      email = (request.body \ "email").as[String]
    )
    val setOp = orgContext.us.setProfile(session.actor, nxProfile).map { ok =>
      val newSession = session.copy(actor = session.actor.copy(profileOpt = Some(nxProfile)))
      Ok(Json.toJson(newSession)).withSession(newSession)
    }
    setOp.onFailure {
      case e: Throwable =>
        Logger.error("Problem setting profile", e)
    }
    setOp
  }

  def listActors = Secure() { session => implicit request =>
    Ok(Json.obj("actorList" -> orgContext.us.listSubActors(session.actor)))
  }

  def createActor() = SecureAsync(parse.json) { session => implicit request =>
    val username = (request.body \ "username").as[String]
    val password = (request.body \ "password").as[String]
    orgContext.us.createSubActor(session.actor, username, password).map { actorOpt =>
      Ok(Json.obj("actorList" -> orgContext.us.listSubActors(session.actor)))
    }
  }

  def deleteActor() = SecureAsync(parse.json) { session => implicit request =>
    val username = (request.body \ "username").as[String]
    orgContext.us.deleteActor(username).map { actorOpt =>
      Ok(Json.obj("actorList" -> orgContext.us.listSubActors(session.actor)))
    }
  }

  def disableActor() = SecureAsync(parse.json) { session => implicit request =>
    val username = (request.body \ "username").as[String]
    orgContext.us.disableActor(username).map { actorOpt =>
      Ok(Json.obj("actorList" -> orgContext.us.listSubActors(session.actor)))
    }
  }

  def enableActor() = SecureAsync(parse.json) { session => implicit request =>
    val username = (request.body \ "username").as[String]
    orgContext.us.enableActor(username).map { actorOpt =>
      Ok(Json.obj("actorList" -> orgContext.us.listSubActors(session.actor)))
    }
  }

  def makeAdmin() = SecureAsync(parse.json) { session => implicit request =>
    val username = (request.body \ "username").as[String]
    orgContext.us.makeAdmin(username).map { actorOpt =>
      Ok(Json.obj("actorList" -> orgContext.us.listSubActors(session.actor)))
    }
  }

  def removeAdmin() = SecureAsync(parse.json) { session => implicit request =>
    val username = (request.body \ "username").as[String]
    orgContext.us.removeAdmin(username).map { actorOpt =>
      Ok(Json.obj("actorList" -> orgContext.us.listSubActors(session.actor)))
    }
  }

  def setPassword() = SecureAsync(parse.json) { session => implicit request =>
    val newPassword = (request.body \ "newPassword").as[String]
    orgContext.us.setPassword(session.actor, newPassword).map(alright => Ok)
  }

  // todo: move this
  def OkFile(file: File, attempt: Int = 0): Result = {
    try {
      val input = new FileInputStream(file)
      val resourceData = Enumerator.fromStream(input)
      val contentType = if (file.getName.endsWith(".json")) "application/json" else "text/plain; charset=utf-8"
      Result(
        ResponseHeader(OK, Map(
          CONTENT_LENGTH -> file.length().toString,
          CONTENT_TYPE -> contentType
        )),
        resourceData
      )
    }
    catch {
      case ex: FileNotFoundException if attempt < 5 => // sometimes status files are in the process of being written
        Thread.sleep(333)
        OkFile(file, attempt + 1)
      case x: Throwable =>
        NotFound(Json.obj("file" -> file.getName, "message" -> x.getMessage))
    }
  }

  // todo: move this
  def OkXml(xml: String): Result = {
    Result(
      ResponseHeader(OK, Map(
        CONTENT_LENGTH -> xml.length().toString,
        CONTENT_TYPE -> "application/xml"
      )),
      body = Enumerator(xml.getBytes)
    )
  }

  /**
    * Returns the JavaScript router that the client can use for "type-safe" routes.
    */
  def jsRoutes(varName: String = "jsRoutes") = Action { implicit request =>
    Ok(
      JavaScriptReverseRouter(varName)(
        routes.javascript.MainController.login,
        routes.javascript.MainController.checkLogin,
        routes.javascript.MainController.logout,
        routes.javascript.MainController.setProfile,
        routes.javascript.MainController.setPassword,
        routes.javascript.MainController.listActors,
        routes.javascript.MainController.createActor,
        routes.javascript.MainController.disableActor,
        routes.javascript.MainController.enableActor,
        routes.javascript.MainController.deleteActor,
        routes.javascript.MainController.makeAdmin,
        routes.javascript.MainController.removeAdmin,
        routes.javascript.AppController.datasetSocket,
        routes.javascript.AppController.listDatasets,
        routes.javascript.AppController.listPrefixes,
        routes.javascript.AppController.createDataset,
        routes.javascript.AppController.setDatasetProperties,
        routes.javascript.AppController.toggleSkosifiedField,
        routes.javascript.AppController.datasetInfo,
        routes.javascript.AppController.command,
        routes.javascript.AppController.nodeStatus,
        routes.javascript.AppController.index,
        routes.javascript.AppController.sample,
        routes.javascript.AppController.histogram,
        routes.javascript.AppController.setRecordDelimiter,
        routes.javascript.AppController.getTermVocabulary,
        routes.javascript.AppController.getTermMappings,
        routes.javascript.AppController.getCategoryMappings,
        routes.javascript.AppController.toggleTermMapping,
        routes.javascript.AppController.getCategoryList,
        routes.javascript.AppController.listSheets,
        routes.javascript.AppController.gatherCategoryCounts,
        routes.javascript.AppController.listSipFiles,
        routes.javascript.AppController.deleteLatestSipFile,
        routes.javascript.AppController.listVocabularies,
        routes.javascript.AppController.createVocabulary,
        routes.javascript.AppController.deleteVocabulary,
        routes.javascript.AppController.setVocabularyProperties,
        routes.javascript.AppController.vocabularyInfo,
        routes.javascript.AppController.vocabularyStatistics,
        routes.javascript.AppController.getSkosMappings,
        routes.javascript.AppController.toggleSkosMapping,
        routes.javascript.AppController.searchVocabulary,
        routes.javascript.AppController.getVocabularyLanguages
      )
    ).as(JAVASCRIPT)
  }

  def testEmail(messageType: String) = Action { request =>
    messageType match {
      case "processing-complete" =>
        Ok(views.html.email.processingComplete(MailProcessingComplete(
          spec = "spec",
          ownerEmailOpt = Some("servers@delving.eu"),
          validString = "1000000000",
          invalidString = "0"
        )))

      case "dataset-error" =>
        var throwable: Throwable = null
        try {
          throw new Exception("This is the exception's message")
        }
        catch {
          case e: Throwable =>
            throwable = e
        }
        Ok(views.html.email.datasetError(MailDatasetError(
          spec = "spec",
          ownerEmailOpt = Some("servers@delving.eu"),
          throwable.getMessage,
          Some(throwable)
        )))

      case badEmailType =>
        Ok(views.html.email.emailNotFound(badEmailType, Seq(
          "processing-complete",
          "dataset-error"
        )))
    }
  }

}
