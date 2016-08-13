package specs

import org.joda.time.{DateTime, DateTimeZone, LocalDateTime}
import org.scalatest.{FlatSpec, Matchers}
import services.Temporal._

class TestTemporal extends FlatSpec with Matchers{

  "date parsing" should "be tolerant" in {

    DateTimeZone.setDefault(DateTimeZone.forID("Europe/Amsterdam"))

    def xform(string: String) = {
      val t = stringToTime(string)
      val s = timeToString(t)
      s
    }

    xform("2014-10-06T07:44:08.854+02:00") should be ("2014-10-06T07:44:08+02:00")
    xform("2014-10-06T07:44:08.854+01:00") should be ("2014-10-06T08:44:08+02:00")
    xform("2014-11-06T07:44:08.854+01:00") should be ("2014-11-06T07:44:08+01:00")
    xform("2014-11-06T07:44:08+01:00") should be ("2014-11-06T07:44:08+01:00")
    xform("2014-11-06T07:44:08Z") should be ("2014-11-06T08:44:08+01:00")
    xform("2014-11-06") should be ("2014-11-06T00:00:00+01:00")

    def utc(string: String) = {
      val t = stringToTime(string)
      val s = timeToUTCString(t)
      s
    }

    utc("2014-11-06T07:44:08.854+01:00") should be ("2014-11-06T06:44:08Z")
    utc("2014-11-06T07:44:08+01:00") should be ("2014-11-06T06:44:08Z")
    utc("2014-11-06T07:44:08Z") should be ("2014-11-06T07:44:08Z")
    utc("2014-11-06") should be ("2014-11-05T23:00:00Z")

    fileNameToLocalString("frans-hals-museum__2014_11_24_16_19__icn.sip.zip") should be("2014-11-24T16:19:00")
    fileNameToLocalString("brabant-collectie-prent__2014_11_17_15_33.sip.zip") should be("2014-11-17T15:33:00")
    fileNameToLocalString("brab_14_7_15_33.sip.zip") should be("UNKNOWN")
  }
}
