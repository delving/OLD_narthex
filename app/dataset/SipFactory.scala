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
package dataset

import java.io.{File, FileInputStream, FileOutputStream}
import java.util.zip.{GZIPOutputStream, ZipEntry, ZipOutputStream}

import dataset.SipRepo.FACTS_FILE
import java.util.HashMap
import org.apache.commons.io.{FileUtils, IOUtils}
import play.api.Logger
import play.api.libs.ws.WSAPI
import triplestore.GraphProperties._

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.xml.NodeSeq

/**
 * @author Gerald de Jong <gerald@delving.eu>
 */

object SipFactory {

  val RECORD_DEFINITION_SUFFIX = "_record-definition.xml"
  val VALIDATION_SUFFIX = "_validation.xsd"

  case class SipGenerationFacts
  (spec: String,
    prefix: String,
    name: String,
    provider: String,
    dataProvider: String,
    dataProviderURL: String,
    language: String,
    rights: String,
    dataType: String,
    orgId: String
    )

  object SipGenerationFacts {
    def apply(info: NodeSeq) = {
      val meta = info \ "metadata"
      val facts = new SipGenerationFacts(
        spec = (info \ "@name").text,
        prefix = (info \ "character" \ "prefix").text,
        name = (meta \ "name").text,
        provider = (meta \ "provider").text,
        dataProvider = (meta \ "dataProvider").text,
        dataProviderURL = (meta \ "dataProviderURL").text,
        language = (meta \ "language").text,
        rights = (meta \ "rights").text,
        dataType = (meta \ "type").text,
        orgId = (meta \ "orgId").text
      )
      val factsString =
        s"""NodeSeq->spec=${facts.spec}
           |name=${facts.name}
           |provider=${facts.provider}
           |dataProvider=${facts.dataProvider}
           |dataProviderURL=${facts.dataProviderURL}
           |language=${facts.language}
           |rights=${facts.rights}
           |type=${facts.dataType}}
           |orgId=${facts.orgId}
           |""".stripMargin
      System.out.println(factsString)
      facts
    }

    def apply(dsInfo: DsInfo) = {
      def info(prop: NXProp) = dsInfo.getLiteralProp(prop).getOrElse("")
      val facts = new SipGenerationFacts(
        spec = dsInfo.spec,
        prefix = info(datasetMapToPrefix),
        name = info(datasetName),
        provider = info(datasetAggregator),
        dataProvider = info(datasetOwner),
        dataProviderURL = info(datasetDataProviderURL),
        language = info(datasetLanguage),
        rights = info(datasetRights),
        dataType = info(datasetType),
        orgId =info(orgId) 
      )
      val factsString =
        s"""DsInfo->spec=${facts.spec}
           |name=${facts.name}
           |provider=${facts.provider}
           |dataProvider=${facts.dataProvider}
           |dataProviderURL=${facts.dataProviderURL}
           |language=${facts.language}
           |rights=${facts.rights}
           |type=${facts.dataType}
           |orgId=${facts.orgId}
           |""".stripMargin
      System.out.println(factsString)
      facts
    }
  }

}

class SipFactory(home: File, rdfBaseUrl: String, wsApi: WSAPI)(implicit ec: ExecutionContext) {

  lazy val prefixRepos = home.listFiles().filter(_.isDirectory).map( home => new SipPrefixRepo(home, rdfBaseUrl, wsApi))

  def prefixRepo(prefix: String) = prefixRepos.find(_.prefix == prefix)

}

class SipPrefixRepo(home: File, rdfBaseUrl: String, ws: WSAPI)(implicit ec: ExecutionContext) {

  import dataset.SipFactory._

  val prefix = home.getName

  lazy val recordDefinition: File = home.listFiles().find(f => f.getName.endsWith(RECORD_DEFINITION_SUFFIX)).getOrElse(
    throw new RuntimeException(s"Missing record definition in $home")
  )

  lazy val validation: File = home.listFiles().find(f => f.getName.endsWith(VALIDATION_SUFFIX)).getOrElse(
    throw new RuntimeException(s"Missing validation xsd in $home")
  )

  lazy val schemaVersions = recordDefinition.getName.substring(0, recordDefinition.getName.length - RECORD_DEFINITION_SUFFIX.length)

  def addFactsEntry(facts: SipGenerationFacts, zos: ZipOutputStream) {
    zos.putNextEntry(new ZipEntry(FACTS_FILE))
    val factsString =
      s"""spec=${facts.spec}
         |name=${facts.name}
         |provider=${facts.provider}
         |dataProvider=${facts.dataProvider}
         |dataProviderURL=${facts.dataProviderURL}
         |language=${facts.language}
         |schemaVersions=$schemaVersions
         |rights=${facts.rights}
         |type=${facts.dataType}
         |baseUrl=${rdfBaseUrl}
         |orgId=${facts.orgId}
         |""".stripMargin
    zos.write(factsString.getBytes("UTF-8"))
    zos.closeEntry()
  }

  def toMap(facts: SipGenerationFacts): HashMap[String,String] = {
    val map = new HashMap[String, String]()
    map.put("spec", facts.spec)
    map.put("name", facts.name)
    map.put("provider", facts.provider)
    map.put("dataProvider", facts.dataProvider)
    map.put("dataProviderURL", facts.dataProviderURL)
    map.put("language", facts.language)
    map.put("schemaVersions", schemaVersions)
    map.put("rights", facts.rights)
    map.put("baseUrl", rdfBaseUrl)
    map.put("orgId", facts.orgId)
    map.put("type", facts.dataType)
    map
  }

  private def differenceWithSchemasDelvingEu(file: File): Option[String] = {
    val fo :Future[Option[String]] = ws.url(s"http://schemas.delving.eu/$prefix/${file.getName}").get().map { response =>
      val fileLines = FileUtils.readFileToString(file).trim.split("\n")
      val onlineLines = response.body.trim.split("\n")
      if (fileLines.size != onlineLines.size) {
        Some(s"$file has ${fileLines.size} lines, while online there are ${onlineLines.size}")
      }
      else {
        val comparisons = fileLines.zip(onlineLines)
        val diffs = comparisons.filter(c => c._1.trim != c._2.trim)
        if (diffs.nonEmpty) {
          Some(diffs.take(3).map(c => s"[${c._1.trim}] != [${c._2.trim}]").mkString("\n"))
        }
        else {
          None
        }
      }
    }
  Await.result(fo, 30.seconds)
  }

  def compareWithSchemasDelvingEu() = {
    differenceWithSchemasDelvingEu(recordDefinition).map(diff => Logger.warn(s"Rec Def Discrepancy: $diff"))
    differenceWithSchemasDelvingEu(validation).map(diff => Logger.warn(s"Validation Discrepancy: $diff"))
  }

  def initiateSipZip(sipFile: File, sourceXmlFile: File, facts: SipGenerationFacts) = {
    val zos = new ZipOutputStream(new FileOutputStream(sipFile))

    addFactsEntry(facts, zos)

    // record definition and validation
    def copyIn(fileToCopy: File) = {
      zos.putNextEntry(new ZipEntry(fileToCopy.getName))
      val fileIn = new FileInputStream(fileToCopy)
      IOUtils.copy(fileIn, zos)
      fileIn.close()
      zos.closeEntry()
    }
    copyIn(recordDefinition)
    copyIn(validation)

    // source, gzipped
    zos.putNextEntry(new ZipEntry(SipRepo.SOURCE_FILE))
    val gzipOut = new GZIPOutputStream(zos)
    val sourceIn = new FileInputStream(sourceXmlFile)
    IOUtils.copy(sourceIn, gzipOut)
    sourceIn.close()
    gzipOut.close()

    zos.close()
  }
}
