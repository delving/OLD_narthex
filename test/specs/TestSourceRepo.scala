package specs

import java.io.{File, FileOutputStream}

import dataset.SourceRepo
import dataset.SourceRepo._
import harvest.Harvesting._
import org.apache.commons.io.FileUtils
import org.scalatest.{FlatSpec, Matchers}
import record.PocketParser._
import services.{FileHandling, ProgressReporter}

import scala.collection.mutable
import scala.xml.XML


class TestSourceRepo extends FlatSpec with Matchers {

  def fresh(dir: String): File = {
    val file = new File(dir)
    FileHandling.clearDir(file)
    file
  }

  def resourceFile(which: String): File = {
    val name = s"source-$which.zip"
    val url = getClass.getResource(s"/source_repo/$name")
    new File(url.getFile)
  }

  def sendProgress(percent: Int): Boolean = true

  "id filters" should "work well for urls" in {

    val hashed = List(
      "one",
      "two",
      "3.1415926535etc",
      "and what about a very long value where nobody really knows when it will end, but we can just guess"
    ).map(SHA256_FILTER.filter)

    val exprectedHashed = List(
      "O2JMHLJVIC5YAPACBM5O4ZWNRCDREMRU5IGG44KDYCW5OP7UGHWQ",
      "H7CMZ7TULBYOFQGZT5Y7GD7QMVWI33OUDTA5PU6TO2YNXZUF4LZQ",
      "JVOZCQPQVUMV6PXC7Q7GM5G4ELITHN7J5JYWGBMTG6SCIMAPMD4Q"
    )

    hashed.zip(exprectedHashed).map { pair =>
      pair._1 should be(pair._2)
    }

    val replaceFilter = IdFilter("replacement", Some("[.].*:::. No more."))

    val replaced = List(
      "Nothing replaced",
      "One sentence. Woops, here is another!"
    ).map(replaceFilter.filter)

    val exprectedReplaced = List(
      "Nothing replaced",
      "One sentence. No more."
    )

    replaced.zip(exprectedReplaced).map { pair =>
      pair._1 should be(pair._2)
    }

  }


}
