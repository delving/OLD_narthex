package services

import java.io.File

import org.apache.commons.io.FileUtils._
import org.scalatest.{FlatSpec, Matchers}
import services.Repo.TermMapping

class TestTermMatching extends FlatSpec with Matchers with TreeHandling with RecordHandling {

    val userHome: String = "/tmp/narthex-user"
    System.setProperty("user.home", userHome)
    deleteQuietly(new File(userHome))

    "A NodeRepo" should "allow terminology mapping" in {
      val repo = Repo("test@narthex.delving.org")
      repo.create("password")
      val fileRepo = repo.fileRepo("pretend-file.xml.gz")

      def createNodeRepo(path: String) = {
        val nodeDir = path.split('/').toList.foldLeft(fileRepo.dir)((file, tag) => new File(file, Repo.tagToDirectory(tag)))
        nodeDir.mkdirs()
        fileRepo.nodeRepo(path).get
      }

      Repo.startBaseX()

      fileRepo.setMapping(TermMapping("a", "http://gumby.com/gumby-is-a-fink", "cusses"))
      fileRepo.setMapping(TermMapping("bb", "http://gumby.com/pokey", "cusses"))
      fileRepo.setMapping(TermMapping("a", "http://gumby.com/gumby", "cusses"))

      fileRepo.getMappings.toString() should be("List(TermMapping(a,http://gumby.com/gumby,cusses), TermMapping(bb,http://gumby.com/pokey,cusses))")
      fileRepo.getMapping("a") should be("http://gumby.com/gumby")
    }

  "A transformer" should "insert an enrichment" in {

    val filePrefix = "gerald_delving_eu/RCE_Beeldbank"
    val recordRoot = "/adlibXML/recordList/record"

    val mappings = Map(
      s"$filePrefix$recordRoot/inner/content.subject/Glas%20in%20loodraam" -> "got it",
      s"$filePrefix$recordRoot/inner/content.subject" -> "close but no cigar"
    )

    val storedString =
      """
        |<narthex id="666" xmlns:very="http://veryother.org/#">
        |  <record>
        |    <inner>
        |      <content.subject>Glas in loodraam</content.subject>
        |    </inner>
        |    <very:other>Ignore</very:other>
        |    <empty/>
        |  </record>
        |</narthex>
      """.stripMargin.trim

    val expectedString =
      """
        |<record xmlns:very="http://veryother.org/#">
        |  <inner>
        |    <content.subject enrichment="got it">Glas in loodraam</content.subject>
        |  </inner>
        |  <very:other>Ignore</very:other>
        |  <empty/>
        |</record>
      """.stripMargin.trim

    val parser = new StoredRecordParser(filePrefix, recordRoot, mappings)
    val record = parser.parse(storedString)
    val recordText = record.text.toString().trim
    recordText should be(expectedString)
  }
}
