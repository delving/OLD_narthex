//===========================================================================
//    Copyright 2014 Delving B.V.
//
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
package dataset

import java.io._
import java.lang.Boolean.FALSE

import com.hp.hpl.jena.query.{Dataset, DatasetFactory}
import org.OrgContext
import org.apache.commons.io.input.CountingInputStream
import org.apache.jena.riot.{RDFDataMgr, RDFFormat}
import org.joda.time.DateTime
import play.api.Logger
import play.api.libs.json.Json
import services.FileHandling.{ReadProgress, clearDir}
import services.StringHandling.createFOAFAbout
import services.{FileHandling, ProgressReporter, StringHandling, Temporal}
import triplestore.GraphProperties._

import scala.collection.JavaConversions._

/**
 * @author Gerald de Jong <gerald@delving.eu>
 */

object ProcessedRepo {

  val XML_SUFFIX = ".xml"
  val ERROR_SUFFIX = ".txt"
  val chunkSize = 100

  case class GraphChunk(dataset: Dataset, dsInfo: DsInfo) {

    def sparqlUpdateQ: String = {

      def singleGraphUpdate(dataset: Dataset, graphUri: String) = {
        val model = dataset.getNamedModel(graphUri)
        val triples = new StringWriter()
        RDFDataMgr.write(triples, model, RDFFormat.NTRIPLES_UTF8)
        s"""
        |DROP SILENT GRAPH <$graphUri>;
        |INSERT DATA { GRAPH <$graphUri> {
        |$triples}};
       """.stripMargin.trim
      }

      dataset.listNames().toList.map(g => singleGraphUpdate(dataset, g)).mkString("\n")
    }

    def bulkAPIQ: String = {

      def createBulkAction(dataset: Dataset, graphUri: String): String = {
        val model = dataset.getNamedModel(graphUri)
        val triples = new StringWriter()
        RDFDataMgr.write(triples, model, RDFFormat.NTRIPLES_UTF8)
        val SpecIdExtractor = "http://.*?/resource/aggregation/([^/]+)/(.+)/graph".r
        val SpecIdExtractor(spec, localId) = graphUri
        val hubId = s"${OrgContext.ORG_ID}_${spec}_$localId"
        val currentSkosFields = dsInfo.getLiteralPropList(skosField)
        val localHash = model.listObjectsOfProperty(model.getProperty(contentHash.uri)).toList().head.toString
        val actionMap = Json.obj(
            "hubId" -> hubId,
            "dataset" -> spec,
            "graphUri" -> graphUri,
            "type" -> "void_EDMRecord",
            "action" -> "index",
            "contentHash" -> localHash.toString,
            "graph" -> s"$triples".stripMargin.trim
        )
        actionMap.toString()
      }
      dataset.listNames().toList.map(g => createBulkAction(dataset, g)).mkString("\n")
    }
  }

  trait GraphReader {
    def isActive: Boolean

    def readChunkOpt: Option[GraphChunk]

    def close(): Unit
  }

  private def numberString(number: Int) = "%05d".format(number)

  private def xmlFileName(number: Int) = s"${numberString(number)}$XML_SUFFIX"

  private def errorFileName(number: Int) = s"${numberString(number)}$ERROR_SUFFIX"

  private def getFileNumber(file: File): Int = file.getName.substring(0, file.getName.indexOf('.')).toInt

  case class ProcessedOutput(home: File, number: Int) {
    val xmlFile = new File(home, xmlFileName(number))
    val errorFile = new File(home, errorFileName(number))
  }

}

class ProcessedRepo(val home: File, dsInfo: DsInfo) {

  import dataset.ProcessedRepo._

  home.mkdir()

  // ===================

  val baseOutput = ProcessedOutput(home, 0)

  def nonEmpty: Boolean = baseOutput.xmlFile.exists()

  def listXmlFiles: List[File] = home.listFiles().filter(f => f.getName.endsWith(XML_SUFFIX)).sortBy(_.getName).toList

  def listOutputs: List[ProcessedOutput] = {
    listXmlFiles.map(file => ProcessedOutput(home, getFileNumber(file))).toList
  }

  def listSourceFiles: List[File] = {
    val sourceHome = new File(home.getParentFile, "/source")
    sourceHome.listFiles().filter(f => f.getName.endsWith(".zip")).sortBy(_.getName).toList
  }

  def createOutput: ProcessedOutput = {
    val number = listOutputs.lastOption.map(_.number + 1).getOrElse(0)
    ProcessedOutput(home, number)
  }

  def getLatestErrors: Option[File] = {
    listOutputs.filter(_.errorFile.exists()).map(_.errorFile).lastOption
  }

  def getLatestProcessed: Option[File] = {
    val files = listOutputs
    files.filter(_.xmlFile.exists()).map(_.xmlFile).lastOption
  }

  def getLatestSourced: Option[File] = {
    val sourceZips = listSourceFiles
    sourceZips.sortBy(_.lastModified).headOption
  }

  def clear() = clearDir(home)

  def createGraphReader(fileOpt: Option[File], timeStamp: DateTime, progressReporter: ProgressReporter) = new GraphReader {
    val LineId = "<!--<([^>]+)__([^>]+)>-->".r
    var files: Seq[File] = fileOpt.map(file => Seq(file)).getOrElse(listXmlFiles)
    val totalLength = (0L /: files.map(_.length()))(_ + _)
    var activeReader: Option[BufferedReader] = None
    var previousBytesRead = 0L
    var activeCounter: Option[CountingInputStream] = None
    val timeString = Temporal.timeToUTCString(timeStamp)

    def activeCount = activeCounter.map(_.getByteCount).getOrElse(0L)

    object ProcessedReadProgress extends ReadProgress {
      override def getPercentRead: Int = {
        val count = previousBytesRead + activeCount
        ((100 * count) / totalLength).toInt
      }
    }

    progressReporter.setReadProgress(ProcessedReadProgress)

    def readerOpt: Option[BufferedReader] =
      if (activeReader.isDefined) {
        activeReader
      }
      else {
        files.headOption.map { file =>
          files = files.tail
          val (reader, counter) = FileHandling.readerCounting(file)
          activeReader = Some(reader)
          activeCounter = Some(counter)
          activeReader.get
        }
      }

    override def isActive = readerOpt.isDefined

    override def readChunkOpt: Option[GraphChunk] = {
      val dataset = DatasetFactory.createMem()
      val recordText = new StringBuilder
      var graphCount = 0
      var chunkComplete = false
      while (!chunkComplete) {
        progressReporter.checkInterrupt()
        readerOpt.map { reader =>
          Option(reader.readLine()).map {
            case LineId(graphName, currentHash) =>
              val m = dataset.getNamedModel(graphName)
              try {
                m.read(new StringReader(recordText.toString()), null, "RDF/XML")
              }
              catch {
                case e: Throwable =>
                  Logger.error(recordText.toString())
                  throw e
              }
              val StringHandling.SubjectOfGraph(subject) = graphName
              val subjectResource = m.getResource(subject)
              m.add(subjectResource, m.getProperty(rdfType), m.getResource(recordEntity))
              val foafAbout = m.getResource(createFOAFAbout(subject))
              m.add(foafAbout, m.getProperty(rdfType), m.getResource(foafDocument))
              m.add(foafAbout, m.getProperty(ccAttributionName), m.createLiteral(dsInfo.spec))
              m.add(foafAbout, m.getProperty(foafPrimaryTopic), subjectResource)
              m.add(foafAbout, m.getProperty(synced.uri), m.createTypedLiteral(FALSE))
              m.add(foafAbout, m.getProperty(contentHash.uri), m.createLiteral(currentHash))
              m.add(foafAbout, m.getProperty(belongsTo.uri), m.getResource(dsInfo.uri))
              m.add(foafAbout, m.getProperty(saveTime.uri), m.createLiteral(timeString))
              graphCount += 1
              recordText.clear()
              if (graphCount >= chunkSize) chunkComplete = true
            case x: String =>
              recordText.append(x).append("\n")
          } getOrElse {
            reader.close()
            previousBytesRead += activeCount
            activeReader = None
            activeCounter = None
          }
        } getOrElse {
          chunkComplete = true
        }
      }
      if (graphCount > 0) Some(GraphChunk(dataset, dsInfo)) else None
    }

    override def close(): Unit = {
      previousBytesRead += activeCount
      activeReader.map(_.close())
      activeReader = None
      activeCounter = None
      files = Seq.empty[File]
    }
  }
}
