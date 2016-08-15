package org

import org.ActorStore.{NXProfile, NXActor}
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