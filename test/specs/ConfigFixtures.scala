package specs

import org.StormPathAuthenticationService.StormPathConfig

object ConfigFixtures {
  val stormpathConfig = StormPathConfig("appId", "key", "secret")

  val BASE = Map(
    "rdfBaseUrl" -> "http://bla",
    "stormpath.appId" -> stormpathConfig.appId,
    "stormpath.key" -> stormpathConfig.apiKey,
    "stormpath.secret" -> stormpathConfig.apiSecret
  )


}
