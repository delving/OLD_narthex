package services

import java.io.File

import actors.ProgressReporter
import org.apache.commons.io.FileUtils
import org.apache.commons.io.FileUtils._
import org.scalatest.{FlatSpec, Matchers}
import services.Harvesting._
import services.RecordHandling.RawRecord

import scala.collection.mutable
import scala.xml.XML


class TestHarvestRepo extends FlatSpec with Matchers with RecordHandling {

  def fresh(dir: String): File = {
    val file = new File(dir)
    deleteQuietly(file)
    file.mkdirs()
    file
  }

  val incoming = fresh("/tmp/test-source-repo-incoming")

  def resourceFile(which: String): File = {
    val name = s"source-$which.zip"
    val url = getClass.getResource(s"/harvest/$name")
    new File(url.getFile)
  }

  def incomingZip(which: String): File = new File(incoming, s"source-$which.zip")

  def sendProgress(percent: Int): Boolean = true

  "A Harvest Repository" should "accept regular record harvest files" in {

    List("a", "b", "c", "d").map(resourceFile).foreach(f => FileUtils.copyFile(f, new File(incoming, f.getName)))
    val recordRoot = "/envelope/list/thing"
    val uniqueId = s"$recordRoot/@which"
    val sourceDir = fresh("/tmp/test-source-repo-regular")

    val harvestRepo = new HarvestRepo(sourceDir, recordRoot, uniqueId, None)

    harvestRepo.countFiles should be(0)
    harvestRepo.acceptZipFile(incomingZip("a"))
    harvestRepo.countFiles should be(3)
    harvestRepo.acceptZipFile(incomingZip("b"))
    harvestRepo.countFiles should be(7)
    harvestRepo.acceptZipFile(incomingZip("c"))
    harvestRepo.countFiles should be(11)
    harvestRepo.acceptZipFile(incomingZip("d"))
    harvestRepo.countFiles should be(16)

    val seenIds = mutable.HashSet[String]()
    var recordCount = 0

    def receiveRecord(record: RawRecord): Unit = {
      //      println(s"${record.id}: ${record.text}")
      if (seenIds.contains(record.id)) fail(s"seen id ${record.id}")
      recordCount += 1
      seenIds.add(record.id)
      val narthex = XML.loadString(record.text)
      val content = (narthex \ "thing" \ "box").text
      content should be("final")
    }

    harvestRepo.parse(receiveRecord, ProgressReporter())

    recordCount should be(4)

    val gitDir = fresh("/tmp/test-source-repo-git")
    val gitFile = new File(gitDir, s"test-source-repo.xml")

    FileHandling.ensureGitRepo(gitDir) should be(true)
    
    harvestRepo.generateSourceFile(gitFile, ProgressReporter(), map => Unit)

    FileHandling.gitCommit(gitFile, "Several words of message") should be(true)
  }

  "A Harvest Repository" should "accept pmh record harvest files" in {

    List("aa", "bb", "cc", "dd").map(resourceFile).foreach(f => FileUtils.copyFile(f, new File(incoming, f.getName)))

    val sourceDir = fresh("/tmp/test-source-repo-pmh")
    val harvestRepo = new HarvestRepo(sourceDir, PMH_RECORD_ROOT, PMH_UNIQUE_ID, Some(PMH_DEEP_RECORD_CONTAINER))

    harvestRepo.countFiles should be(0)
    harvestRepo.acceptZipFile(incomingZip("aa"))
    harvestRepo.countFiles should be(3)
    harvestRepo.acceptZipFile(incomingZip("bb"))
    harvestRepo.countFiles should be(7)
    harvestRepo.acceptZipFile(incomingZip("cc"))
    harvestRepo.countFiles should be(11)
    harvestRepo.acceptZipFile(incomingZip("dd"))
    harvestRepo.countFiles should be(16)

    val seenIds = mutable.HashSet[String]()
    var recordCount = 0

    def receiveRecord(record: RawRecord): Unit = {
      //      println(s"${record.id}: ${record.text}")
      if (seenIds.contains(record.id)) fail(s"seen id ${record.id}")
      recordCount += 1
      seenIds.add(record.id)
      val narthex = XML.loadString(record.text)
      val content = (narthex \ "thing" \ "box").text
      content should be("final")
    }

    harvestRepo.parse(receiveRecord, ProgressReporter())

    recordCount should be(4)
  }


}
