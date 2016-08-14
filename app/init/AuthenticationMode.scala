package init

object AuthenticationMode extends Enumeration {

  val PROPERTY_NAME = "authenticationMode"

  type AuthenticationMode = Value
  val MOCK, TS, STORMPATH = Value

  def fromConfigString(valueOpt: Option[String]): AuthenticationMode = {
    valueOpt match {
      case None => throw new RuntimeException("authenticationMode not specified")
      case Some(v) => v match {
        case "mock" => AuthenticationMode.MOCK
        case "ts" => AuthenticationMode.TS
        case "stormpath" => AuthenticationMode.STORMPATH
        case _ => throw new RuntimeException(s"Unknown authentication mode specified")
      }
    }
  }
}
