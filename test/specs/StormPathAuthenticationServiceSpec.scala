package specs

import mockws.MockWS
import org.ActorStore.NXActor
import org.StormPathAuthenticationService
import org.scalatest.FunSuite
import org.scalatestplus.play.OneAppPerSuite
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.mvc.Results._
import play.api.test.FakeApplication
import play.api.test.Helpers._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}


/**
  * Intended to check out json parsing and producing
  */
class StormPathAuthenticationServiceSpec extends FunSuite with OneAppPerSuite {

  import scala.concurrent.ExecutionContext.Implicits.global

  implicit override lazy val app: FakeApplication = FakeApplication(additionalConfiguration = ConfigFixtures.BASE)

  val stormpathConfig = ConfigFixtures.stormpathConfig

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

    val eventualMaybeActor: Future[Option[NXActor]] = authService.authenticate("foo", "bar")
    val maybeActor: Option[NXActor] = Await.result(eventualMaybeActor, 2.seconds)

    assert(maybeActor.isEmpty)
  }

  test("user exists, successful login") {
    val jsonValue =
      s"""
         |{
         |        "account": {
         |            "href": "$hrefBase$validAccountId"
         |        }
         |    }
    """.stripMargin

    val accountDetailUrl = s"${StormPathAuthenticationService.STORMPATH_BASE_URL}accounts/$validAccountId"

    val username = "foo"
    val accountDetailsResponse =
      s"""
         |{
         |  "username": "$username",
         |  "email": "email",
         |  "givenName": "first",
         |  "surname": "last"
         |}
       """.stripMargin

    val ws = MockWS {
      case (POST, `loginUrl`) => Action {
        Ok(jsonValue)
      }
      case (GET, `accountDetailUrl`) => Action {
        Ok(accountDetailsResponse)
      }
    }
    val authService = new StormPathAuthenticationService(ws, stormpathConfig)


    val eventualMaybeNXActor: Future[Option[NXActor]] = authService.authenticate(username, "bar")
    val maybeActor: Option[NXActor] = Await.result(eventualMaybeNXActor, 2.seconds)

    assert(maybeActor.isDefined)
    assert(maybeActor.get.actorName == username)
  }
}
