//===========================================================================
//    Copyright 2014 Delving B.V.
//
//    Licensed under the Apache License, Version 2.0 (the "License");
//    you may not use this file except in compliance with the License.
//    You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
//    Unless required by applicable law or agreed to in writing, software
//    distributed under the License is distributed on an "AS IS" BASIS,
//    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//    See the License for the specific language governing permissions and
//    limitations under the License.
//===========================================================================

package services

import java.io.{FileReader, BufferedReader, File}
import play.api.Play.current
import play.api.libs.concurrent.Akka
import java.util.UUID
import play.api.libs.json.{JsValue, Json}
import org.mindrot.jbcrypt.BCrypt
import org.apache.commons.io.FileUtils._
import scala.collection.mutable.ArrayBuffer
import org.basex.server.ClientSession
import actors._
import play.api.libs.json.JsArray
import actors.SaveRecords
import scala.Some
import play.api.libs.json.JsObject

object Repo {
  val SUFFIXES = List(".xml.gz", ".xml")
  val home = new File(System.getProperty("user.home"))
  val root = new File(home, "NARTHEX")
  var baseXDir = new File(root, "basex")

  lazy val baseX: BaseX = new BaseX("localhost", 6789, 6788, "admin", "admin", false)

  def apply(email: String) = new Repo(root, email)

  def startBaseX() = {
    baseX.start(baseXDir)
  }

  def stopBaseX() = {
    baseX.stop()
  }

  def tagToDirectory(tag: String) = tag.replace(":", "_").replace("@", "_")

  def acceptableFile(fileName: String, contentType: Option[String]) = {
    // todo: something with content-type
    println("content type " + contentType)
    !SUFFIXES.filter(suffix => fileName.endsWith(suffix)).isEmpty
  }

  def createJson(file: File, content: JsObject) = {
    writeStringToFile(file, Json.prettyPrint(content), "UTF-8")
  }

  def updateJson(file: File)(xform: JsValue => JsObject) = {
    if (file.exists()) {
      val value = Json.parse(readFileToString(file))
      val tempFile = new File(file.getParentFile, s"${file.getName}.temp")
      writeStringToFile(tempFile, Json.prettyPrint(xform(value)), "UTF-8")
      deleteQuietly(file)
      moveFile(tempFile, file)
    }
    else {
      writeStringToFile(file, Json.prettyPrint(xform(Json.obj())), "UTF-8")
    }
  }

  def getSuffix(fileName: String) = {
    val suffix = SUFFIXES.filter(suf => fileName.endsWith(suf))
    if (suffix.isEmpty) "" else suffix.head
  }

  def stripSuffix(fileName: String) = {
    val suffix = getSuffix(fileName)
    fileName.substring(0, fileName.length - suffix.length)
  }
}

class Repo(root: File, val email: String) {

  val personalRootName: String = email.replaceAll("[@.]", "_")
  val personalRoot = new File(root, personalRootName)
  val user = new File(personalRoot, "user.json")
  val uploaded = new File(personalRoot, "uploaded")
  val analyzed = new File(personalRoot, "analyzed")

  def create(password: String) = {
    personalRoot.mkdirs()
    val passwordHash = BCrypt.hashpw(password, BCrypt.gensalt())
    writeStringToFile(user, Json.prettyPrint(Json.obj("passwordHash" -> passwordHash)))
  }

  def authenticate(password: String) = {
    if (user.exists()) {
      val userObject = Json.parse(readFileToString(user))
      val passwordHash = (userObject \ "passwordHash").as[String]
      BCrypt.checkpw(password, passwordHash)
    }
    else {
      false
    }
  }

  def uploadedFile(fileName: String) = {
    val suffix = Repo.getSuffix(fileName)

    if (suffix.isEmpty) {
      val fileNameDot = s"$fileName."
      var matchingFiles = uploaded.listFiles().filter(file => file.getName.startsWith(fileNameDot))
      if (matchingFiles.isEmpty) {
        new File(uploaded, "nonexistent")
      }
      else {
        matchingFiles.head
      }
    }
    else {
      new File(uploaded, fileName)
    }
  }

  def analyzedDir(fileName: String) = new File(analyzed, Repo.stripSuffix(fileName))

  def listUploadedFiles = listFiles(uploaded)

  def listFileRepos = listUploadedFiles.map(file => Repo.stripSuffix(file.getName))

  def uploadedOnly() = listUploadedFiles.filter(file => !analyzedDir(file.getName).exists())

  def scanForWork() = {
    val files = uploadedOnly()
    val dirs = files.map(file => analyzedDir(file.getName))
    val pairs = files.zip(dirs)
    val fileAnalysisDirs = pairs.map(pair => new FileRepo(this, pair._2.getName, pair._1, pair._2).mkdirs)
    val jobs = files.zip(fileAnalysisDirs)
    jobs.foreach {
      job =>
        val analyzer = Akka.system.actorOf(Analyzer.props(job._2), job._1.getName)
        analyzer ! Analyzer.Analyze(job._1)
    }
    files
  }

  def FileRepo(fileName: String) = new FileRepo(this, Repo.stripSuffix(fileName), uploadedFile(fileName), analyzedDir(fileName))

  def createDatabase[T](block: ClientSession => T): T = {
    Repo.baseX.createDatabase(personalRootName)
    Repo.baseX.withSession(personalRootName)(block)
  }

  private def listFiles(directory: File): List[File] = {
    if (directory.exists()) {
      directory.listFiles.filter(file =>
        file.isFile && !Repo.SUFFIXES.filter(suffix => file.getName.endsWith(suffix)).isEmpty
      ).toList
    }
    else {
      List.empty
    }
  }

}

class FileRepo(val personalRepo: Repo, val name: String, val sourceFile: File, val dir: File) {

  def mkdirs = {
    dir.mkdirs()
    this
  }

  def index = new File(dir, "index.json")

  def status = new File(dir, "status.json")

  def setStatus(content: JsObject) = Repo.createJson(status, content)

  def root = new NodeRepo(this, dir)

  def saveRecords(recordRoot: String, uniqueId: String) = {
    val saver = Akka.system.actorOf(Saver.props(this), name)
    saver ! SaveRecords(recordRoot, uniqueId, name)
  }

  def status(path: String): Option[File] = {
    nodeRepo(path) match {
      case None => None
      case Some(nodeDirectory) => Some(nodeDirectory.status)
    }
  }

  def sample(path: String, size: Int): Option[File] = {
    nodeRepo(path) match {
      case None => None
      case Some(repo) =>
        val fileList = repo.sampleJson.filter(pair => pair._1 == size)
        if (fileList.isEmpty) None else Some(fileList.head._2)
    }
  }

  def histogram(path: String, size: Int): Option[File] = {
    nodeRepo(path) match {
      case None => None
      case Some(repo) =>
        val fileList = repo.histogramJson.filter(pair => pair._1 == size)
        if (fileList.isEmpty) None else Some(fileList.head._2)
    }
  }

  def indexText(path: String): Option[File] = {
    nodeRepo(path) match {
      case None => None
      case Some(repo) => Some(repo.indexText)
    }
  }

  def uniqueText(path: String): Option[File] = {
    nodeRepo(path) match {
      case None => None
      case Some(repo) => Some(repo.uniqueText)
    }
  }

  def histogramText(path: String): Option[File] = {
    nodeRepo(path) match {
      case None => None
      case Some(repo) => Some(repo.histogramText)
    }
  }

  def nodeRepo(path: String): Option[NodeRepo] = {
    val nodeDir = path.split('/').toList.foldLeft(dir)((file, tag) => new File(file, Repo.tagToDirectory(tag)))
    if (nodeDir.exists()) Some(new NodeRepo(this, nodeDir)) else None
  }

}

object NodeRepo {
  def apply(parent: FileRepo, parentDir: File, tag: String) = {
    val dir = if (tag == null) parentDir else new File(parentDir, Repo.tagToDirectory(tag))
    dir.mkdirs()
    new NodeRepo(parent, dir)
  }
}

class NodeRepo(val parent: FileRepo, val dir: File) {

  def child(childTag: String) = NodeRepo(parent, dir, childTag)

  def f(name: String) = new File(dir, name)

  def status = f("status.json")

  def setStatus(content: JsObject) = Repo.createJson(status, content)

  def values = f("values.txt")

  def tempSort = f(s"sorting-${UUID.randomUUID()}.txt")

  def sorted = f("sorted.txt")

  def counted = f("counted.txt")

  val sizeFactor = 5 // relates to the lists below

  def histogramJson = List(100, 500, 2500).map(size => (size, f(s"histogram-$size.json")))

  def sampleJson = List(100, 500, 2500).map(size => (size, f(s"sample-$size.json")))

  def indexText = f("index.txt")

  def uniqueText = f("unique.txt")

  def histogramText = f("histogram.txt")

  def writeHistograms(uniqueCount: Int) = {

    val LINE = """^ *(\d*) (.*)$""".r
    val input = new BufferedReader(new FileReader(histogramText))

    def lineOption = {
      val string = input.readLine()
      if (string != null) Some(string) else None
    }

    def createFile(maximum: Int, entries: ArrayBuffer[JsArray], histogramFile: File) = {
      Repo.createJson(histogramFile, Json.obj(
        "uniqueCount" -> uniqueCount,
        "entries" -> entries.size,
        "maximum" -> maximum,
        "complete" -> (entries.size == uniqueCount),
        "histogram" -> entries
      ))
    }

    var activeCounters = histogramJson.map(pair => (pair._1, new ArrayBuffer[JsArray], pair._2))
    activeCounters = activeCounters.filter(pair => pair._1 == activeCounters.head._1 || uniqueCount > pair._1 / sizeFactor)
    val counters = activeCounters
    var line = lineOption
    var count = 1
    while (line.isDefined && !activeCounters.isEmpty) {
      val lineMatch = LINE.findFirstMatchIn(line.get)
      activeCounters = activeCounters.filter {
        triple =>
          lineMatch.map(groups => triple._2 += Json.arr(groups.group(1), groups.group(2)))
          val keep = count < triple._1
          if (!keep) createFile(triple._1, triple._2, triple._3) // side effect
          keep
      }
      line = lineOption
      count += 1
    }
    activeCounters.foreach(triple => createFile(triple._1, triple._2, triple._3))
    counters.map(triple => triple._1)

  }

}
