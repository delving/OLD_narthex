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

import java.io.File

import akka.pattern.ask
import akka.util.Timeout
import analysis.NodeRepo
import dataset.DatasetActor.{StartAnalysis, StartHarvest, StartSaving}
import dataset.DatasetOrigin.HARVEST
import dataset.DatasetState._
import dataset.ProgressState._
import harvest.Harvester.{HarvestAdLib, HarvestPMH}
import harvest.Harvesting.HarvestType._
import harvest.Harvesting._
import harvest.{HarvestRepo, Harvesting}
import org.OrgActor.{DatasetMessage, InterruptDataset}
import org.{OrgActor, OrgRepo}
import play.api.Logger
import play.api.Play.current
import play.api.cache.Cache
import record.RecordHandling.{StoredRecord, TargetConcept}
import record.{RecordDb, RecordHandling}
import services.FileHandling.clearDir
import services._

import scala.concurrent.Await

class DatasetRepo(val orgRepo: OrgRepo, val name: String) extends RecordHandling {

  val rootDir = new File(orgRepo.datasetsDir, name)
  val incomingDir = new File(rootDir, "incoming")
  val analyzedDir = new File(rootDir, "analyzed")
  val harvestDir = new File(rootDir, "harvest")

  val rootNode = new NodeRepo(this, analyzedDir)

  val dbName = s"narthex_${orgRepo.orgId}___$name"
  lazy val datasetDb = new DatasetDb(orgRepo.repoDb, name)
  lazy val termDb = new TermDb(dbName)
  lazy val recordDb = new RecordDb(this, dbName)

  override def toString = name

  def mkdirs = {
    rootDir.mkdirs()
    incomingDir.mkdir()
    analyzedDir.mkdir()
    harvestDir.mkdir()
    this
  }

  def createIncomingFile(fileName: String) = new File(incomingDir, fileName)

  def getLatestIncomingFile = incomingDir.listFiles().toList.sortBy(_.lastModified()).lastOption

  def firstHarvest(harvestType: HarvestType, url: String, dataset: String, prefix: String) = datasetDb.infoOption map { info =>
    DatasetState.fromDatasetInfo(info).foreach { state =>
      if (state == EMPTY) {
        clearDir(harvestDir) // it's a first harvest
        clearDir(analyzedDir) // just in case
        datasetDb.startProgress(HARVESTING)
        datasetDb.setOrigin(HARVEST)
        datasetDb.setHarvestInfo(harvestType, url, dataset, prefix)
        datasetDb.setHarvestCron(Harvesting.harvestCron(info)) // a clean one
        datasetDb.setRecordDelimiter(harvestType.recordRoot, harvestType.uniqueId)
        Logger.info(s"First Harvest $name")
        OrgActor.actor ! DatasetMessage(name, StartHarvest(None))
      }
      else {
        Logger.warn(s"Harvest can only be started in $EMPTY, not $state")
      }
    }
  }

  def nextHarvest() = datasetDb.infoOption.map { info =>
    val recordsTimeOption = fromXSDDateTime(info \ "records" \ "time")
    recordsTimeOption.map { recordsTime =>
      val harvestCron = Harvesting.harvestCron(info)
      if (harvestCron.timeToWork) {
        val nextHarvestCron = harvestCron.next
        datasetDb.setHarvestCron(if (nextHarvestCron.timeToWork) harvestCron.now else nextHarvestCron)
        val harvest = info \ "harvest"
        HarvestType.fromString((harvest \ "harvestType").text).map {
          harvestType =>
            val harvestRepo = new HarvestRepo(harvestDir, harvestType)
            val kickoff = harvestType match {
              case PMH =>
                HarvestPMH(
                  url = (harvest \ "url").text,
                  set = (harvest \ "dataset").text,
                  prefix = (harvest \ "prefix").text,
                  modifiedAfter = Some(harvestCron.previous)
                )
              case ADLIB =>
                HarvestAdLib(
                  url = (harvest \ "url").text,
                  database = (harvest \ "dataset").text,
                  modifiedAfter = Some(harvestCron.previous)
                )
            }
            Logger.info(s"Re-harvest $kickoff")
            datasetDb.startProgress(HARVESTING)
            OrgActor.actor ! DatasetMessage(name, kickoff)
          //                OrgActor.create(Harvester.props(this, harvestRepo), name, harvester => harvester ! kickoff)
        } getOrElse {
          Logger.warn(s"No re-harvest $name with cron $harvestCron because harvest type was not recognized $harvest")
        }
      }
      else {
        Logger.info(s"No re-harvest of $name with cron $harvestCron because it's not time $harvestCron")
      }
    } getOrElse {
      Logger.warn(s"Incremental harvest of $name can only be started when there are saved records")
    }
  }

  def startAnalysis() = datasetDb.infoOption.map { info =>
    DatasetState.fromDatasetInfo(info).map { state =>
      if (state == SOURCED) {
        clearDir(analyzedDir)
        datasetDb.startProgress(ANALYZING)
        OrgActor.actor ! DatasetMessage(name, StartAnalysis())
      }
      else {
        Logger.warn(s"Analyzing $name can only be started when the state is sourced, but it's $state")
      }
    }
  }

  def firstSaveRecords() = datasetDb.infoOption.map { info =>
    DatasetState.fromDatasetInfo(info).map { state =>
      val delimited = datasetDb.isDelimited(Some(info))
      if (state == SOURCED && delimited) {
        recordDb.createDb()
        datasetDb.startProgress(SAVING)
        OrgActor.actor ! DatasetMessage(name, StartSaving(None))
      }
      else {
        Logger.warn(s"First save of $name can only be started with state sourced/delimited, but it's $state/$delimited ")
      }
    }
  }

  def revertState: DatasetState = {
    val currentState = datasetDb.infoOption.flatMap { info =>
      val maybe = DatasetState.fromDatasetInfo(info)
      if (maybe.isEmpty) Logger.warn(s"No current state?? $info")
      maybe
    } getOrElse DELETED
    Logger.info(s"Revert state of $name from $currentState")
    datasetDb.infoOption.map { info =>
      def toRevertedState: DatasetState = currentState match {
        case DELETED =>
          datasetDb.createDataset(EMPTY)
          EMPTY
        case EMPTY =>
          datasetDb.setStatus(DELETED)
          DELETED
        case SOURCED =>
          val treeTimeOption = fromXSDDateTime(info \ "tree" \ "time")
          val recordsTimeOption = fromXSDDateTime(info \ "records" \ "time")
          if (treeTimeOption.isEmpty && recordsTimeOption.isEmpty) {
            clearDir(incomingDir)
            datasetDb.setStatus(EMPTY)
            EMPTY
          }
          else {
            SOURCED // no change
          }
      }
      implicit val timeout = Timeout(100L)
      val answer = OrgActor.actor ? InterruptDataset(name)
      val interrupted = Await.result(answer, timeout.duration).asInstanceOf[Boolean]
      if (interrupted) currentState else toRevertedState
    } getOrElse currentState
  }

  def index = new File(analyzedDir, "index.json")

  def nodeRepo(path: String): Option[NodeRepo] = {
    val nodeDir = path.split('/').toList.foldLeft(analyzedDir)((file, tag) => new File(file, OrgRepo.pathToDirectory(tag)))
    if (nodeDir.exists()) Some(new NodeRepo(this, nodeDir)) else None
  }

  def status(path: String): Option[File] = nodeRepo(path).map(_.status)

  def sample(path: String, size: Int): Option[File] = {
    nodeRepo(path) match {
      case None => None
      case Some(nodeRepo) =>
        val fileList = nodeRepo.sampleJson.filter(pair => pair._1 == size)
        if (fileList.isEmpty) None else Some(fileList.head._2)
    }
  }

  def histograms(path: String, size: Int): Option[File] = nodeRepo(path).map { repo =>
    val fileList = repo.histogramJson.filter(pair => pair._1 == size)
    fileList.headOption.map(_._2)
  } getOrElse None

  def histogram(path: String, size: Int): Option[File] = nodeRepo(path).map { repo =>
    val fileList = repo.histogramJson.filter(pair => pair._1 == size)
    fileList.headOption.map(_._2)
  } getOrElse None

  def indexText(path: String): Option[File] = nodeRepo(path).map(_.indexText)

  def uniqueText(path: String): Option[File] = nodeRepo(path).map(_.uniqueText)

  def histogramText(path: String): Option[File] = nodeRepo(path).map(_.histogramText)

  def enrichRecords(storedRecords: String): List[StoredRecord] = {
    val pathPrefix = s"${NarthexConfig.ORG_ID}/$name"
    val mappings = Cache.getAs[Map[String, TargetConcept]](name).getOrElse {
      val freshMap = termDb.getMappings.map(m => (m.source, TargetConcept(m.target, m.vocabulary, m.prefLabel))).toMap
      Cache.set(name, freshMap, 60 * 5)
      freshMap
    }
    val parser = new StoredRecordEnricher(pathPrefix, mappings)
    parser.parse(storedRecords)
  }

  def invalidateEnrichmentCache() = Cache.remove(name)
}
