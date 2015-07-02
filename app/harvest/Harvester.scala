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

package harvest

import java.io.{File, FileOutputStream}
import java.util.zip.{ZipEntry, ZipOutputStream}

import akka.actor.{Actor, Props}
import akka.pattern.pipe
import dataset.DatasetActor._
import dataset.DatasetContext
import harvest.Harvester.{HarvestAdLib, HarvestComplete, HarvestPMH, IncrementalHarvest}
import harvest.Harvesting.{AdLibHarvestPage, HarvestError, PMHHarvestPage}
import org.OrgContext.actorWork
import org.apache.commons.io.FileUtils
import play.api.Logger
import services.ProgressReporter.ProgressState._
import services.{ProgressReporter, StringHandling}

import scala.concurrent._
import scala.language.postfixOps

object Harvester {

  case class HarvestAdLib(strategy: HarvestStrategy, url: String, database: String, search: String)

  case class HarvestPMH(strategy: HarvestStrategy, url: String, set: String, prefix: String)

  case class IncrementalHarvest(strategy: HarvestStrategy, fileOpt: Option[File])

  case class HarvestComplete(incrementalOpt: Option[IncrementalHarvest])

  def props(datasetContext: DatasetContext) = Props(new Harvester(datasetContext))
}

class Harvester(val datasetContext: DatasetContext) extends Actor with Harvesting {

  import context.dispatcher

  val log = Logger
  var tempFile = File.createTempFile("narthex-harvest", ".zip")
  val zip = new ZipOutputStream(new FileOutputStream(tempFile))
  var pageCount = 0
  var progressOpt: Option[ProgressReporter] = None

  def addPage(page: String): Int = {
    val harvestPageName = s"harvest_$pageCount.xml"
    zip.putNextEntry(new ZipEntry(harvestPageName))
    zip.write(page.getBytes("UTF-8"))
    zip.closeEntry()
    pageCount += 1
    pageCount
  }

  def finish(strategy: HarvestStrategy, errorOpt: Option[String]) = {
    zip.close()
    log.info(s"finished harvest strategy=$strategy error=$errorOpt")
    errorOpt match {
      case Some(message) =>
        context.parent ! WorkFailure(message)
      case None =>
        strategy match {
          case ModifiedAfter(_, _) =>
            datasetContext.sourceRepoOpt match {
              case Some(sourceRepo) =>
                future {
                  val acceptZipReporter = ProgressReporter(COLLECTING, context.parent)
                  val fileOption = sourceRepo.acceptFile(tempFile, acceptZipReporter)
                  log.info(s"Zip file accepted: $fileOption")
                  context.parent ! HarvestComplete(Some(IncrementalHarvest(strategy, fileOption)))
                } onFailure {
                  case e: Exception =>
                    context.parent ! WorkFailure(e.getMessage)
                }
              case None =>
                context.parent ! WorkFailure(s"No source repo for $datasetContext")
            }
          case _ =>
            context.parent ! HarvestComplete(None)
        }
    }
  }

  def handleFailure(future: Future[Any], strategy: HarvestStrategy, message: String) = {
    future.onFailure {
      case e: Exception =>
        log.warn(s"Harvest failure", e)
        finish(strategy, Some(e.toString))
    }
  }

  def receive = {

    // http://umu.adlibhosting.com/api/wwwopac.ashx?xmltype=grouped&limit=50&database=collect&search=modification%20greater%20%272014-12-01%27

    case HarvestAdLib(strategy, url, database, search) => actorWork(context) {
      log.info(s"Harvesting $url $database to $datasetContext")
      val futurePage = fetchAdLibPage(strategy, url, database, search)
      handleFailure(futurePage, strategy, "adlib harvest")
      strategy match {
        case Sample =>
          val rawXml = datasetContext.createRawFile(StringHandling.slugify(url))
          futurePage.map {
            case page: AdLibHarvestPage =>
              FileUtils.writeStringToFile(rawXml, page.records, "UTF-8")
              finish(strategy, None)
          }
        case _ =>
          progressOpt = Some(ProgressReporter(HARVESTING, context.parent))
          // todo: if None comes back there's something wrong
          futurePage pipeTo self
      }
    }

    case AdLibHarvestPage(records, url, database, search, strategy, diagnostic) => actorWork(context) {
      val pageNumber = addPage(records)
      strategy match {
        case ModifiedAfter(_, _) =>
          progressOpt.get.sendPage(pageNumber) // This compensates for AdLib's failure to report number of hits
        case _ =>
          progressOpt.get.sendPercent(diagnostic.percentComplete)
      }
      log.info(s"Harvest Page: $pageNumber - $url $database to $datasetContext: $diagnostic")
      if (diagnostic.isLast) {
        finish(strategy, None)
      }
      else {
        val futurePage = fetchAdLibPage(strategy, url, database, search, Some(diagnostic))
        handleFailure(futurePage, strategy, "adlib harvest page")
        futurePage pipeTo self
      }
    }

    case HarvestPMH(strategy: HarvestStrategy, url, set, prefix) => actorWork(context) {
      log.info(s"Harvesting $url $set $prefix to $datasetContext")
      val futurePage = fetchPMHPage(strategy, url, set, prefix)
      handleFailure(futurePage, strategy, "pmh harvest")
      strategy match {
        case Sample =>
          val rawXml = datasetContext.createRawFile(StringHandling.slugify(url))
          futurePage.map {
            case page: PMHHarvestPage =>
              FileUtils.writeStringToFile(rawXml, page.records, "UTF-8")
              finish(strategy, None)
          }
        case _ =>
          progressOpt = Some(ProgressReporter(HARVESTING, context.parent))
          // todo: if None comes back there's something wrong
          futurePage pipeTo self
      }
    }

    case PMHHarvestPage(records, url, set, prefix, total, strategy, resumptionToken) => actorWork(context) {
      val pageNumber = addPage(records)
      log.info(s"Harvest Page $pageNumber to $datasetContext: $resumptionToken")
      resumptionToken.map { token =>
        if (token.hasPercentComplete) {
          progressOpt.get.sendPercent(token.percentComplete)
        }
        else {
          progressOpt.get.sendPage(pageCount)
        }
        val futurePage = fetchPMHPage(strategy, url, set, prefix, resumptionToken)
        handleFailure(futurePage, strategy, "pmh harvest page")
        futurePage pipeTo self
      } getOrElse {
        finish(strategy, None)
      }
    }

    case HarvestError(error, strategy) =>
      finish(strategy, Some(error))

  }
}