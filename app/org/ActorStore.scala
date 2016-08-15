//===========================================================================
//    Copyright 2015 Delving B.V.
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

package org

import java.security.MessageDigest

import org.OrgContext._
import play.api.Logger
import triplestore.Sparql._
import triplestore.TripleStore

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

object ActorStore {

  case class NXProfile(firstName: String, lastName: String, email: String) {
    val nonEmpty = if (firstName.isEmpty && lastName.isEmpty && email.isEmpty) None else Some(this)
  }

  case class NXActor(userNameProposed: String, makerOpt: Option[String], profileOpt: Option[NXProfile] = None) {
    val actorName = userNameProposed.replaceAll("[^\\w-]", "").toLowerCase
    val uri = s"$NX_URI_PREFIX/actor/$actorName"

    override def toString = uri
  }

}

class ActorStore(val authenticationService: AuthenticationService)(implicit ec: ExecutionContext, ts: TripleStore) {

  import org.ActorStore._

  val topActorUsername = "admin"

  def insertAdmin(passwd: String): NXActor = {
    val topActor = NXActor(topActorUsername, None)
    ts.up.sparqlUpdate(insertTopActorQ(topActor, Utils.hashPasswordUnsecure(passwd, topActorUsername))).map(ok => Some(topActor))
    Logger.info(s"Created initial admin user")
    topActor
  }

  def hasAdmin: Boolean = {
    val result: Future[Option[NXActor]] =
      ts.query(getActor(topActorUsername))
        .map(m => actorFromResult(m))
    val actorOpt: Option[NXActor] = Await.result(result, 5.seconds)
    actorOpt.isDefined
  }


  def emailFromUri(actorUri: String): Option[String] = {
    val query = ts.query(getEMailOfActor(actorUri)).map(emailFromResult)
    // todo: maybe make this async
    Await.result(query, 5.seconds)
  }

  def adminEmails: List[String] = {
    val sparqlQuery = getAdminEMailQ()
    val query = ts.query(sparqlQuery).map(emailsFromResult)
    // todo: maybe make this async
    Await.result(query, 5.seconds)
  }

  def authenticate(actorName: String, password: String): Future[Option[NXActor]] = {
    authenticationService.authenticate(actorName, password)
  }

  def listSubActors(nxActor: NXActor): List[Map[String, String]] = {
    val query = ts.query(getSubActorList(nxActor)).map { list =>
      list.map(m =>
        Map(
          "userName" -> m.get("username").get.text,
          "isAdmin" -> {
            val isAdmin = m.get("isAdmin")
            if (isAdmin.nonEmpty) isAdmin.get.text else "false"
          },
          "userEnabled" -> {
            val actorEnabled = m.get("actorEnabled")
            if (actorEnabled.nonEmpty) actorEnabled.get.text else "true"
          }
        )
      )
    }
    // todo: maybe make this async
    Await.result(query, 5.seconds)
  }

  def createSubActor(adminActor: NXActor, usernameString: String, password: String): Future[Option[NXActor]] = {
    val hash = Utils.hashPasswordUnsecure(password, usernameString)
    val newActor = NXActor(usernameString, Some(adminActor.uri))
    val update = ts.up.sparqlUpdate(insertSubActorQ(newActor, hash, adminActor))
    checkFail(update)
    update.map(ok => Some(newActor))
  }

  def makeAdmin(userName: String): Future[Option[NXActor]] = {
    val actor = NXActor(userName, None, None)
    val q = setActorAdminQ(actor, true)
    val update = ts.up.sparqlUpdate(q)
    checkFail(update)
    update.map(ok => Some(actor))
  }

  def removeAdmin(userName: String): Future[Option[NXActor]] = {
    val actor = NXActor(userName, None, None)
    val q = setActorAdminQ(actor, false)
    val update = ts.up.sparqlUpdate(q)
    checkFail(update)
    update.map(ok => Some(actor))
  }

  def deleteActor(userName: String): Future[Option[NXActor]] = {
    val actor = NXActor(userName, None, None)
    val q = removeActorQ(actor)
    val update = ts.up.sparqlUpdate(q)
    checkFail(update)
    update.map(ok => Some(actor))
  }

  def disableActor(userName: String): Future[Option[NXActor]] = {
    val actor = NXActor(userName, None, None)
    val q = enableActorQ(actor, false)
    val update = ts.up.sparqlUpdate(q)
    checkFail(update)
    update.map(ok => Some(actor))
  }

  def enableActor(userName: String): Future[Option[NXActor]] = {
    val actor = NXActor(userName, None, None)
    val q = enableActorQ(actor, true)
    val update = ts.up.sparqlUpdate(q)
    checkFail(update)
    update.map(ok => Some(actor))
  }

  def setProfile(actor: NXActor, nxProfile: NXProfile): Future[Unit] = {
    val q = setActorProfileQ(actor, nxProfile)
    val update = ts.up.sparqlUpdate(q)
    checkFail(update)
    update
  }

  def setPassword(actor: NXActor, newPassword: String): Future[Boolean] = {
    val hash = Utils.hashPasswordUnsecure(newPassword, actor.actorName)
    val update = ts.up.sparqlUpdate(setActorPasswordQ(actor, hash))
    checkFail(update)
    update.map(ok => true)
  }

  private def checkFail(futureUnit: Future[Unit]) = futureUnit.onFailure {
    case e: Throwable =>
      Logger.error("Problem with ActorStore", e)
  }
}
