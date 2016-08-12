package specs

import org.StormPathAuthenticationService
import org.StormPathAuthenticationService.StormPathConfig
import org.scalatest.AsyncFunSuite
import mockws.MockWS
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.mvc.Results._
import play.api.test.Helpers._


/**
  * Intended to check out json parsing and producing
  */
class StormPathAuthenticationServiceSpec extends AsyncFunSuite {

  val stormpathConfig = StormPathConfig("appId", "key", "secret")
  val loginUrl = s"${StormPathAuthenticationService.STORMPATH_BASE_URL}applications/${stormpathConfig.appId}/loginAttempts"

  val validAccountId = "myValidId"
  val hrefBase = s"${StormPathAuthenticationService.STORMPATH_BASE_URL}accounts/"

  test("no user returned when stormpath says no") {
    val invalidLoginResponse = Json.obj(
      "status" -> 400,
      "code" -> 400,
      "message" -> "Invalid username or password.",
      "developerMessage" -> "Invalid username or password.",
      "moreInfo" -> "mailto:support@stormpath.com"
    )

    val ws = MockWS {
      case (POST, u) if u == s"${StormPathAuthenticationService.STORMPATH_BASE_URL}applications/${stormpathConfig.appId}/loginAttempts" => Action {
        Ok(invalidLoginResponse)
      }
    }
    val authService = new StormPathAuthenticationService(ws, stormpathConfig)

    authService.authenticate("foo", "bar").map {
      actorOpt => assert(!actorOpt.isDefined)
    }
  }

  test("user exists") {
    val jsonValue = s"""
     |{
     |        "account": {
     |            "href": "${hrefBase}${validAccountId}"
     |        }
     |    }
    """.stripMargin

    val accountDetailUrl = s"${StormPathAuthenticationService.STORMPATH_BASE_URL}accounts/$validAccountId"

    val accountDetailsResponse =
      s"""
         |
       """.stripMargin

    val ws = MockWS {
      case (POST, loginUrl) => Action {
        Ok(jsonValue)
      }
      case (GET, accountDetailUrl) => Action {
        Ok(accountDetailsResponse)
      }
    }
    val authService = new StormPathAuthenticationService(ws, stormpathConfig)
    authService.authenticate("foo", "bar").map {
      actorOpt => assert(actorOpt.isDefined)
    }
  }
}
