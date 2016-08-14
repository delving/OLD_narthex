import init.AuthenticationMode
import org.OrgContext._
import play.api._

object Global extends GlobalSettings {

  override def beforeStart(app: Application) {
    val authMode = AuthenticationMode.fromConfigString(app.configuration.getString(AuthenticationMode.PROPERTY_NAME))
    Logger.info(s"Narthex initializing for ${orgContext.orgId}, authMode: $authMode")
  }

  override def onStop(app: Application) {
    Logger.info("Shutting down tickers")
    harvestTicker.cancel()
//    skosifyTicker.cancel()
    Logger.info("Narthex shutdown...")
  }
}
