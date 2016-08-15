import init.AuthenticationMode
import org.ActorStore.NXActor
import org.OrgContext._
import play.api._


object Global extends GlobalSettings {

  val topActorConfigProp = "topActor.initialPassword"

  override def beforeStart(app: Application) {
    val authMode = AuthenticationMode.fromConfigString(app.configuration.getString(AuthenticationMode.PROPERTY_NAME))
    Logger.info(s"Narthex initializing for ${orgContext.orgId}, authMode: $authMode")
  }


  override def onStart(app: Application) {
    val actorStore = orgContext.us
    val initialPassword: String = config.getString(topActorConfigProp).
      getOrElse(throw new RuntimeException(s"${topActorConfigProp} not set"))
    if (!actorStore.hasAdmin) {
      val topActorConfigProp = "topActor.initialPassword"
      val admin: NXActor = actorStore.insertAdmin(initialPassword)
      Logger.info(s"Inserted initial admin user, details: ${admin}")
    }
  }

  override def onStop(app: Application) {
    Logger.info("Shutting down tickers")
    harvestTicker.cancel()
//    skosifyTicker.cancel()
    Logger.info("Narthex shutdown...")
  }
}
