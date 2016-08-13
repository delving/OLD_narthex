package specs

import java.io.File

import org.scalatest.{FlatSpec, Matchers}
import services.FileHandling
import services.StringHandling._

class TestStringHandling extends FlatSpec with Matchers {

  "string handling" should "read csv" in {

    val csvDir = new File(getClass.getResource("/csv").getFile)
    val csvFiles = csvDir.listFiles.filter(_.getName.endsWith("csv"))

    csvFiles.foreach { csvFile =>
      val name = csvFile.getName
      val reader = FileHandling.createReader(csvFile)
      val xmlFile = new File(csvFile.getParent, s"$name.xml")
      val writer = FileHandling.createWriter(xmlFile)
      csvToXML(reader, writer)
    }
  }
}
