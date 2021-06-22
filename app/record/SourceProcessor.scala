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

package record

import java.io.{File, FileOutputStream, StringReader, StringWriter}
import org.apache.jena.query.{Dataset, DatasetFactory}
import org.apache.jena.riot.{RDFDataMgr, RDFFormat}
import akka.actor.{Actor, ActorLogging, Props}
import dataset.DatasetActor.{Scheduled, WorkFailure}
import dataset.DatasetContext
import dataset.SipFactory.SipGenerationFacts
import dataset.SipRepo.URIErrorsException
import eu.delving.groovy.DiscardRecordException
import organization.OrgContext
import nxutil.Utils.actorWork
import triplestore.GraphProperties.datasetType
import org.apache.commons.io.FileUtils.deleteQuietly
import org.joda.time.{DateTime, LocalDateTime}
import org.xml.sax.SAXException
import record.PocketParser.Pocket
import record.SourceProcessor._
import services.FileHandling._
import services.ProgressReporter.ProgressState._
import services.Temporal._
import services.{FileHandling, ProgressReporter}
import play.api.libs.json.{JsValue, Json, Writes}


import scala.language.postfixOps
import scala.util.{Failure, Success}

object SourceProcessor {

  case class AdoptSource(file: File, orgContext: OrgContext)

  case class SourceAdoptionComplete(file: File)

  case object GenerateSipZip

  case class SipZipGenerationComplete(recordCount: Int)

  case class Process(scheduledOpt: Option[Scheduled])

  case class ProcessingComplete(validRecords: Int,
                                invalidRecords: Int,
                                scheduledOpt: Option[Scheduled])

  def props(datasetContext: DatasetContext, orgContext: OrgContext) =
    Props(new SourceProcessor(datasetContext, orgContext))

}

class SourceProcessor(val datasetContext: DatasetContext,
                      orgContext: OrgContext)
    extends Actor
    with ActorLogging {

  var progress: Option[ProgressReporter] = None
  val dsInfo = datasetContext.dsInfo

  def receive = {

    case AdoptSource(file, orgContext) =>
      actorWork(context) {
        log.info(s"Adopt source ${file.getAbsolutePath}")
        datasetContext.sourceRepoOpt match {
          case Some(sourceRepo) =>
            val progressReporter = ProgressReporter(ADOPTING, context.parent)
            progress = Some(progressReporter)
            sourceRepo.acceptFile(file, progressReporter) match {
              case Some(adoptedFile) =>
                context.parent ! SourceAdoptionComplete(adoptedFile)
              case None =>
                context.parent ! WorkFailure(
                  s"File not accepted: ${file.getAbsolutePath}")
            }
          case None =>
            context.parent ! WorkFailure("Missing source repository!")
        }
      }

    case GenerateSipZip =>
      actorWork(context) {
        log.info("Generate SipZip")
        datasetContext.sourceRepoOpt.map { sourceRepo =>
          val progressReporter = ProgressReporter(GENERATING, context.parent)
          progress = Some(progressReporter)
          val pocketFile = datasetContext.pocketFile
          pocketFile.getParentFile.mkdirs()
          val pocketOutput = new FileOutputStream(pocketFile)
          val idFilter = dsInfo.getIdFilter
          val pocketCount =
            sourceRepo.generatePockets(pocketOutput, idFilter, progressReporter)
          val sipFileOpt: Option[File] = datasetContext.sipRepo.latestSipOpt
            .map { latestSip =>
              val prefixRepoOpt = latestSip.sipMappingOpt.flatMap(mapping =>
                datasetContext.orgContext.sipFactory.prefixRepo(mapping.prefix))
              datasetContext.sipFiles.foreach(_.delete())
              val sipFile = datasetContext.createSipFile
              pocketOutput.close()
              latestSip.copyWithSourceTo(sipFile, pocketFile, prefixRepoOpt, SipGenerationFacts(dsInfo))
              Some(sipFile)
            } getOrElse {
            val facts = SipGenerationFacts(dsInfo)
            val sipPrefixRepo =
              datasetContext.orgContext.sipFactory.prefixRepo(facts.prefix)
            sipPrefixRepo.map { prefixRepo =>
              datasetContext.sipFiles.foreach(_.delete())
              val sipFile = datasetContext.createSipFile
              pocketOutput.close()
              prefixRepo.initiateSipZip(sipFile, pocketFile, facts)
              Some(sipFile)
            } getOrElse {
              pocketOutput.close()
              context.parent ! WorkFailure("Unable to build sip for download")
              None
            }
          }
          if (pocketCount > 0) {
            context.parent ! SipZipGenerationComplete(pocketCount)
          } else {
            sipFileOpt.map(_.delete())
            deleteQuietly(datasetContext.pocketFile)
            context.parent ! WorkFailure(
              "Zero pockets generated. You probably forgot te set the record root and unique identifier.")
          }
        } getOrElse {
          context.parent ! WorkFailure("No data for generating SipZip")
        }
      }

    case Process(scheduledOpt) =>
      actorWork(context) {
        val sourceFacts = datasetContext.sourceRepoOpt
          .map(_.sourceFacts)
          .getOrElse(
            throw new RuntimeException(s"No source facts for $datasetContext"))
        val sipMapper = datasetContext.sipMapperOpt.getOrElse(
          throw new RuntimeException(s"No sip mapper for $datasetContext"))

        // TODO: enable again when incremental harvesting is supported
        //if (scheduledOpt.isEmpty) datasetContext.processedRepo.clear()
        datasetContext.processedRepo.clear()

        val processedOutput = datasetContext.processedRepo.createOutput
        val xmlOutput = createWriter(processedOutput.xmlFile)
        val errorOutput = createWriter(processedOutput.errorFile)
        val bulkActionOutput = createWriter(processedOutput.bulkActionFile)
        //val nquadOutput = createWriter(processedOutput.nquadFile)
        val harvestingLogger = appender(datasetContext.harvestLogger)
        var validRecords = 0
        var invalidRecords = 0
        var time = System.currentTimeMillis()
        val dataset = DatasetFactory.createGeneral()

        def writeError(heading: String, error: String, id: String) = {
          invalidRecords += 1
          val errorString = s"""
                         |===== $invalidRecords: $heading === ( $id )=====
                         |$error
             """.stripMargin.trim
          if (!scheduledOpt.isEmpty) {
            harvestingLogger.write(errorString)
            harvestingLogger.newLine()
          }
          errorOutput.write(errorString)
          errorOutput.write("\n")
        }

        def writeBulkAction(rdf: String, graphUri: String, localId: String) = {
          val model = dataset.getNamedModel(graphUri)
          try {
            model.read(new StringReader(rdf), null, "RDF/XML")
          } catch {
            case e: Throwable =>
              errorOutput.write(rdf)
              errorOutput.write("\n")
              throw e
          }
          //val model = dataset.getNamedModel(graphUri)
          val triples = new StringWriter()
          RDFDataMgr.write(triples, model, RDFFormat.JSONLD_FLAT)

          val orgId = dsInfo.orgId
          val spec = dsInfo.spec
          val hubId = s"${orgId}_${spec}_$localId"
          //val localHash = model.listObjectsOfProperty(model.getProperty(contentHash.uri)).toList().head.toString
          val actionMap = Json.obj(
            "hubId" -> hubId,
            "orgId" -> orgId,
            "localId" -> localId,
            "dataset" -> spec,
            "graphUri" -> graphUri,
            "type" -> dsInfo.getLiteralProp(datasetType).getOrElse("narthex_record").toString(),
            "action" -> "index",
            "graphMimeType" -> "application/ld+json",
            //"contentHash" -> localHash.toString,
            "graph" -> s"$triples".stripMargin.trim
          )
          bulkActionOutput.write(actionMap.toString())
          bulkActionOutput.write("\n")
          // TODO make sure the named graph is part of the model
          //var nquads = new StringWriter()
          //RDFDataMgr.write(nquads, model, RDFFormat.NQUADS)
          //nquadOutput.write(s"$nquads".stripMargin.trim)
          //nquadOutput.write("\n")
        }

        def catchPocket(rawPocket: Pocket): Unit = {
          progress.get.checkInterrupt()
          val startAll = System.currentTimeMillis()
          val pocketTry = sipMapper.executeMapping(rawPocket)
          val processing = System.currentTimeMillis() - startAll
          val startWrite = System.currentTimeMillis()
          pocketTry match {
            case Success(pocket) =>
              pocket.writeTo(xmlOutput)
              // insert RDF graph into bulk actions
              // TODO make bulk action switchable
              //writeBulkAction(pocket.text, pocket.id, rawPocket.id)
              validRecords += 1

            case Failure(ue: URIErrorsException) =>
              writeError("URI ERRORS",
                         ue.uriErrors.mkString("\n"),
                         rawPocket.id)

            case Failure(disc: DiscardRecordException) =>
              writeError("DISCARDED RECORD", disc.getMessage, rawPocket.id)

            case Failure(sax: SAXException) =>
              writeError("XSD ERROR", sax.getMessage, rawPocket.id)

            case Failure(unexpected: Throwable) =>
              writeError("UNEXPECTED ERROR", unexpected.toString, rawPocket.id)
          }
          val total = validRecords + invalidRecords
          if (total % 10000 == 0) {
            val now = System.currentTimeMillis()
            log.debug(s"Processing $total: ${now - time}ms")
            time = now
          }
          val writing = System.currentTimeMillis() - startWrite
          val totalTime = System.currentTimeMillis() - startAll
          log.debug(
            s"Total: ${totalTime}. Processing: ${processing}, writing: $writing")
        }

        val idFilter = dsInfo.getIdFilter

        scheduledOpt.map { incremental =>
          log.info(s"Processing incremental $sourceFacts")
          val (source, readProgress) =
            FileHandling.sourceFromFile(incremental.file)
          try {

            val progressReporter = ProgressReporter(PROCESSING, context.parent)
            progressReporter.setReadProgress(readProgress)
            progress = Some(progressReporter)
            val parser = new PocketParser(sourceFacts, idFilter, orgContext)
            parser.parse(source,
                         Set.empty[String],
                         catchPocket,
                         progressReporter)
          } finally {
            source.close()
          }
        } getOrElse {
          log.info(s"Processing all $sourceFacts")
          datasetContext.sourceRepoOpt.map { sourceRepo =>
            val progressReporter = ProgressReporter(PROCESSING, context.parent)
            progress = Some(progressReporter)
            sourceRepo.parsePockets(catchPocket, idFilter, progressReporter)
          }
        }

        xmlOutput.close()
        errorOutput.close()
        bulkActionOutput.close()
        //nquadOutput.close()
        if (invalidRecords == 0) deleteQuietly(processedOutput.errorFile)
        val scheduledOptOutput = if (!scheduledOpt.isEmpty) {
          Some(scheduledOpt.get.copy(file = processedOutput.xmlFile))
        } else {
          scheduledOpt
        }
        harvestingLogger.write(
          s"Processed Source Records - ${LocalDateTime.now()} - valid records: $validRecords - invalid records: $invalidRecords - scheduled: ${!scheduledOpt.isEmpty}")
        harvestingLogger.newLine()
        harvestingLogger.close()
        context.parent ! ProcessingComplete(validRecords,
                                            invalidRecords,
                                            scheduledOptOutput)
      }
  }

}
