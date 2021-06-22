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

import java.io._
import java.net.URL
import java.util.zip.{GZIPOutputStream, ZipEntry, ZipFile, ZipOutputStream}
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamSource
import javax.xml.validation.Validator

import eu.delving.XMLToolFactory
import eu.delving.groovy._
import eu.delving.metadata._
import dataset.SipFactory.SipGenerationFacts
import eu.delving.schema.SchemaVersion
import org.apache.commons.io.{FileUtils, IOUtils}
import org.joda.time.DateTime
import org.w3c.dom.Element
import play.api.Logger
import record.PocketParser._
import services.StringHandling
import services.StringHandling.urlEncodeValue

import scala.collection.JavaConversions._
import scala.io.Source
import scala.util.Try

/**
 * A repository of sip files which knows everything about what a sip means, revealing all of the contained
 * attributes from the contained property files and able to produce a SipMapper which can transform pockets
 * of data according to the SIP-Creator mappings.
 *
 * @author Gerald de Jong <gerald@delving.eu>
 */

object SipRepo {

  val SOURCE_FILE = "source.xml.gz"
  val FACTS_FILE = "narthex_facts.txt"
  val HINTS_FILE = "hints.txt"
  val MAX_ZIP_COUNT = 6
  val SchemaVersionsPattern = "schemaVersions=(.*)".r
  val SourcePattern = "source.*".r
  val MappingPattern = "mapping_.*".r
  val RecordDefinitionPattern = ".*_record-definition.xml".r
  val ValidationPattern = ".*_validation.xsd".r

  val SIP_EXTENSION = ".sip.zip"

  case class AvailableSip(file: File) {
    val n = file.getName
    if (!n.endsWith(SIP_EXTENSION)) throw new RuntimeException(s"Strange file name $file")
    val datasetName = n.substring(0, n.indexOf("__"))
    val dateTime = new DateTime(file.lastModified())
  }

  class URIErrorsException(val uriErrors: List[String]) extends Exception

}

class SipRepo(home: File, spec: String, rdfBaseUrl: String) {

  def createSipZipFile(sipZipFileName: String) = {
    home.mkdir()
    new File(home, sipZipFileName)
  }

  def listSips: Seq[Sip] = {
    if (home.exists()) {
      val filesLastToFirst = home.listFiles().filter(_.getName.endsWith(".sip.zip")).sortBy(_.lastModified())
      val filesLimited =
        if (filesLastToFirst.length <= SipRepo.MAX_ZIP_COUNT)
          filesLastToFirst
        else {
          filesLastToFirst.head.delete()
          filesLastToFirst.tail
        }
      filesLimited.reverse.map(Sip(spec, rdfBaseUrl, _))
    }
    else Seq.empty[Sip]
  }

  def latestSipOpt: Option[Sip] = listSips.headOption

  override def toString = spec
}

object Sip {

  // Note: this is set in the OrgContext but we don't want a dependency from there (requires app for testing)
  val XSD_VALIDATION = System.getProperty("XSD_VALIDATION", "true") == "true"

  case class SipMapping(spec: String, prefix: String, version: String,
                        recDefTree: RecDefTree, validatorOpt: Option[Validator],
                        recMapping: RecMapping, sip: Sip) {

    def namespaces: Map[String, String] = recDefTree.getRecDef.namespaces.map(ns => ns.prefix -> ns.uri).toMap

    def fileName = s"mapping_$prefix.xml"

    def close() = sip.close()
  }

  trait SipMapper {

    val datasetName: String

    val prefix: String

    def executeMapping(pocket: Pocket): Try[Pocket]

  }

  val PrefixVersion = "(.*)_(.*)".r

  def apply(datasetName: String, rdfBaseUrl: String, zipFile: File) = new Sip(datasetName, rdfBaseUrl, zipFile)

  val XMLNS = "http://www.w3.org/2000/xmlns/"
  val RDF_ROOT_TAG: String = "RDF"
  val RDF_RECORD_TAG: String = "Description"
  val RDF_ABOUT_ATTRIBUTE: String = "about"
  val RDF_PREFIX: String = "rdf"
  val RDF_URI: String = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

  val INPUT_METADATA = "/input/metadata/"

}

class Sip(val dsInfoSpec: String, rdfBaseUrl: String, val file: File) {

  import dataset.Sip._
  import dataset.SipRepo._

  lazy val zipFile = new ZipFile(file)

  lazy val entries = zipFile.entries().map(entry => entry.getName -> entry).toMap

  def close() = {
    zipFile.close()
  }

  def readMap(propertyFileName: String): Map[String, String] = {
    entries.get(propertyFileName).map { entry =>
      val inputStream = zipFile.getInputStream(entry)
      val lines = Source.fromInputStream(inputStream, "UTF-8").getLines()
      val propertyMap = lines.flatMap { line =>
        val equals = line.indexOf("=")
        if (equals < 0) None else Some(line.substring(0, equals).trim -> line.substring(equals + 1).trim)
      }.toMap
      inputStream.close()
    return propertyMap
    } getOrElse (throw new RuntimeException(s"No entry for $propertyFileName of $dsInfoSpec"))
  }

  lazy val facts = readMap(FACTS_FILE)

  def fact(name: String): Option[String] = facts.get(name)

  lazy val spec = fact("spec")
  lazy val name = fact("name")
  lazy val orgId = fact("orgId")
  lazy val provider = fact("provider")
  lazy val dataProvider = fact("dataProvider")
  lazy val dataProviderURL = fact("dataProviderURL")
  lazy val country = fact("country")
  lazy val language = fact("language")
  lazy val rights = fact("rights")
  lazy val dataType = fact("type")
  lazy val schemaVersions = fact("schemaVersions")

  lazy val hints = readMap(HINTS_FILE)

  private def hint(name: String): Option[String] = hints.get(name)

  lazy val harvestType = hint("harvestType")
  lazy val harvestUrl = hint("harvestUrl")
  lazy val harvestSpec = hint("harvestSpec")
  lazy val harvestPrefix = hint("harvestPrefix")
  lazy val recordCount = hint("recordCount")
  lazy val recordRootPath = hint("recordRootPath")
  lazy val uniqueElementPath = hint("uniqueElementPath")
  lazy val pocketsHint = hint("pockets")

  if (pocketsHint.isEmpty) Logger.warn(s"Narthex hints did not define pockets")

  lazy val schemaVersionOpt: Option[String] = schemaVersions.flatMap(commas => commas.split(" *,").headOption)

  private def recDefTree(sipContentFileName: String): RecDefTree = {
    entries.get(sipContentFileName).map { entry =>
      val inputStream = zipFile.getInputStream(entry)
      val recDef = RecDef.read(inputStream)
      val tree = RecDefTree.create(recDef)
      inputStream.close()
      return tree

    } getOrElse (throw new RuntimeException(s"Unable to read rec def $sipContentFileName from $file"))
  }

  private def recMapping(sipContentFileName: String, recDefTree: RecDefTree) = {
    entries.get(sipContentFileName).map { entry =>
      val inputStream = zipFile.getInputStream(entry)
      val mapping = RecMapping.read(inputStream, recDefTree)
      inputStream.close()
      mapping
    } getOrElse (throw new RuntimeException(s"Unable to read rec def $sipContentFileName from $file"))
  }

  private class ResolverContext extends CachedResourceResolver.Context {

    override def get(urlString: String): String = {
      try {
        val url = new URL(urlString)
        IOUtils.toString(url.openStream(), "UTF-8");
      }
      catch {
        case e: Exception =>
          throw new RuntimeException("Problem fetching", e);
      }
    }

    override def file(systemId: String): File = {
      val cache = new File("/tmp")
      new File(cache, systemId.replaceAll("[/:]", "_"))
    }
  }

  private def validator(sipContentFileName: String, prefix: String): Option[Validator] = {
    entries.get(sipContentFileName).map { entry =>
      val inputStream = zipFile.getInputStream(entry)
      val schemaFactory = XMLToolFactory.schemaFactory(prefix)
      val context = new ResolverContext()
      val resolver = new CachedResourceResolver(context)
      schemaFactory.setResourceResolver(resolver)
      val source: StreamSource = new StreamSource(inputStream)
      val schema = schemaFactory.newSchema(source)
      val newValidator = schema.newValidator()
      inputStream.close()
      return Some(newValidator)
    }
  }

  lazy val sipMappingOpt: Option[SipMapping] = schemaVersionOpt.map { schemaVersion =>
    val PrefixVersion(prefix, version) = schemaVersion
    val tree = recDefTree(s"${schemaVersion}_record-definition.xml")
    val mapping = recMapping(s"mapping_$prefix.xml", tree)
    SipMapping(
      spec = dsInfoSpec,
      prefix = prefix,
      version = version,
      recDefTree = tree,
      recMapping = mapping,
      validatorOpt = if (XSD_VALIDATION) validator(s"${schemaVersion}_validation.xsd", prefix) else None,
      sip = this
    )
  }

  def containsSource = entries.containsKey("source.xml.gz")

  def copySourceToTempFile: Option[File] = {
    entries.get("source.xml.gz").map { sourceXmlGz =>
      val sourceFile = new File(file.getParentFile, "source.xml.gz")
      val inputStream = zipFile.getInputStream(sourceXmlGz)
      FileUtils.copyInputStreamToFile(inputStream, sourceFile)
      inputStream.close()
      sourceFile
    }
  }

  class MappingEngine(sipMapping: SipMapping) extends SipMapper {
    val now = new DateTime
    val serializer = new XmlSerializer
    val namespaces = sipMapping.recDefTree.getRecDef.namespaces.map(ns => ns.prefix -> ns.uri).toMap
    val factory = new MetadataRecordFactory(namespaces)

    val runner = new BulkMappingRunner(sipMapping.recMapping, new CodeGenerator(sipMapping.recMapping).withTrace(false).toRecordMappingCode)

    override val datasetName = sipMapping.spec

    override val prefix: String = sipMapping.prefix

    override def executeMapping(pocket: Pocket): Try[Pocket] = Try {
      val metadataRecord = factory.metadataRecordFrom(pocket.getText)
      val result = new MappingResult(serializer, pocket.id, runner.runMapping(metadataRecord),
        sipMapping.recMapping.getRecDefTree)
      // check uri errors
      val uriErrors = result.getUriErrors.toList
      if (uriErrors.nonEmpty) throw new URIErrorsException(uriErrors)
      // validate using XSD
      sipMapping.validatorOpt.foreach(_.validate(new DOMSource(result.root())))
      // re-wrap in an RDF construction
      val root = result.root().asInstanceOf[Element]
      val doc = root.getOwnerDocument
      root.removeAttribute("xsi:schemaLocation")
      val cn = root.getChildNodes
      val kids = for (index <- 0 to (cn.getLength - 1)) yield cn.item(index)
      val (rootNode, graphName) = prefix match {
        case "edm" =>
          val rdfWrapper = doc.createElementNS(RDF_URI, s"$RDF_PREFIX:$RDF_ROOT_TAG")
          kids.foreach(rdfWrapper.appendChild)
          val aggregation = kids.find(node => node.getLocalName == "Aggregation").getOrElse(throw new RuntimeException(s"No ore:aggregation found!"))
          val about = aggregation.getAttributes.getNamedItemNS(RDF_URI, RDF_ABOUT_ATTRIBUTE)
          val aggregationUri = about.getTextContent
          (rdfWrapper, StringHandling.createGraphName(aggregationUri))
        case "naa" =>
          val rdfWrapper = doc.createElementNS(RDF_URI, s"$RDF_PREFIX:$RDF_ROOT_TAG")
          kids.foreach(rdfWrapper.appendChild)
          val aggregation = kids.find(node => node.getLocalName == "RecordAggregation").getOrElse(kids.head)
          val about = aggregation.getAttributes.getNamedItemNS(RDF_URI, RDF_ABOUT_ATTRIBUTE)
          val aggregationUri = about.getTextContent
          (rdfWrapper, StringHandling.createGraphName(aggregationUri))
        case "nao" =>
          val rdfWrapper = doc.createElementNS(RDF_URI, s"$RDF_PREFIX:$RDF_ROOT_TAG")
          kids.foreach(rdfWrapper.appendChild)
          val aggregation = kids.find(node => node.getLocalName == "Recordaggregatie").getOrElse(kids.head)
          val about = aggregation.getAttributes.getNamedItemNS(RDF_URI, RDF_ABOUT_ATTRIBUTE)
          val aggregationUri = about.getTextContent
          (rdfWrapper, StringHandling.createGraphName(aggregationUri))

        case _ =>
          val rdfElement = doc.createElementNS(RDF_URI, s"$RDF_PREFIX:$RDF_RECORD_TAG")
          val rdfAbout = s"$rdfBaseUrl/resource/graph/$datasetName/${urlEncodeValue(pocket.id)}"
          rdfElement.setAttributeNS(RDF_URI, s"$RDF_PREFIX:$RDF_ABOUT_ATTRIBUTE", rdfAbout)
          kids.foreach(rdfElement.appendChild)
          val graphName = rdfAbout
          (rdfElement, graphName)
      }
      // deliver the pocket
      val xml = serializer.toXml(rootNode, true).replaceFirst("<[?].*[?]>\n", "")
      Pocket(graphName, xml, sipMapping.namespaces + (RDF_PREFIX -> RDF_URI))
    }
  }

  def createSipMapper: Option[SipMapper] = sipMappingOpt.map(new MappingEngine(_))

  def copyWithSourceTo(sipFile: File, sourceXmlFile: File, sipPrefixRepoOpt: Option[SipPrefixRepo], facts: SipGenerationFacts) = {
    val zos = new ZipOutputStream(new FileOutputStream(sipFile))
    var sourceFound = false

    def copyFileIn(sourceFile: File, fileName: String, gzip: Boolean) = {
      zos.putNextEntry(new ZipEntry(fileName))
      val in = new FileInputStream(sourceFile)
      if (gzip) {
        val out = new GZIPOutputStream(zos)
        IOUtils.copy(in, out)
        out.finish()
      }
      else {
        IOUtils.copy(in, zos)
      }
      in.close()
      zos.closeEntry()
    }

    sipPrefixRepoOpt.map { prefixRepo =>
      prefixRepo.addFactsEntry(facts, zos)
    }

    zipFile.entries.foreach { entry =>

      def copyEntry(): Unit = {
        zos.putNextEntry(new ZipEntry(entry.getName))
        val is = zipFile.getInputStream(entry)
        IOUtils.copy(is, zos)
        zos.closeEntry()
        is.close()
      }

      entry.getName match {

        case MappingPattern() =>
          Logger.debug(s"Mapping: ${entry.getName}")
          sipMappingOpt.foreach { sipMapping =>
            zos.putNextEntry(new ZipEntry(sipMapping.fileName))
            // if there is a new schema version, set it
            sipPrefixRepoOpt.foreach(sipPrefix => sipMapping.recMapping.setSchemaVersion(new SchemaVersion(sipPrefix.schemaVersions)))

            sipPrefixRepoOpt.map { prefixRepo =>
              val factsMap = prefixRepo.toMap(facts)
              for(factEntry <- factsMap.entrySet()) {
                sipMapping.recMapping.setFact(factEntry.getKey(), factEntry.getValue())
              }
            }
            RecMapping.write(zos, sipMapping.recMapping)
            zos.closeEntry()
          }

        case RecordDefinitionPattern() =>
          Logger.debug(s"Record definition: ${entry.getName}")
          sipPrefixRepoOpt.map(_.recordDefinition).map(rd => copyFileIn(rd, rd.getName, gzip = false)).getOrElse(copyEntry())

        case ValidationPattern() =>
          Logger.debug(s"Validation: ${entry.getName}")
          sipPrefixRepoOpt.map(_.validation).map(va => copyFileIn(va, va.getName, gzip = false)).getOrElse(copyEntry())

        case SourcePattern() =>
          Logger.debug(s"Source: ${entry.getName}")
          sourceFound = true
          copyFileIn(sourceXmlFile, entry.getName, gzip = true)

        case FACTS_FILE =>
          // Do not copy

        case entryName =>
          Logger.debug(s"Verbatim: ${entry.getName}")
          copyEntry()
      }
    }

    if (!sourceFound) {
      Logger.debug(s"Source added afterwards")
      copyFileIn(sourceXmlFile, SOURCE_FILE, gzip = true)
    }

    zos.close()
  }

}
