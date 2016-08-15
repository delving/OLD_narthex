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

package org

import java.io.File
import java.util

import akka.actor.ActorContext
import dataset.DatasetActor.WorkFailure
import dataset.DsInfo.withDsInfo
import dataset.SipRepo.{AvailableSip, SIP_EXTENSION}
import dataset._
import harvest.PeriodicHarvest
import harvest.PeriodicHarvest.ScanForHarvests
import init.AuthenticationMode
import mapping._
import org.ActorStore.NXActor
import org.OrgActor.DatasetsCountCategories
import play.api.{Logger, Play}
import play.libs.Akka._
import services.FileHandling.clearDir
import services.StringHandling.urlEncodeValue
import triplestore.GraphProperties.categoriesInclude
import triplestore.{Fuseki, TripleStore}

import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.postfixOps

object OrgContext {
  val config = Play.current.configuration

  def configFlag(name: String): Boolean = config.getBoolean(name).getOrElse(false)

  def configString(name: String) = config.getString(name).getOrElse(
    throw new RuntimeException(s"Missing config string: $name")
  )

  def configStringNoSlash(name: String) = configString(name).replaceAll("\\/$", "")

  def configInt(name: String) = config.getInt(name).getOrElse(
    throw new RuntimeException(s"Missing config int: $name")
  )

  def secretList(name: String): util.List[String] = config.getStringList(name).getOrElse(List("secret"))

  val USER_HOME = System.getProperty("user.home")
  val NARTHEX = new File(USER_HOME, "NarthexFiles")

  lazy val API_ACCESS_KEYS = secretList("api.accessKeys")

  lazy val HARVEST_TIMEOUT = config.getInt("harvest.timeout").getOrElse(3 * 60 * 1000)

  def apiKeyFits(accessKey: String) = API_ACCESS_KEYS.contains(accessKey)

  val ORG_ID = configString("orgId")
  val NARTHEX_DOMAIN = configStringNoSlash("domains.narthex")
  val NAVE_DOMAIN = configStringNoSlash("domains.nave")
  val NAVE_API_URL = configStringNoSlash("naveApiUrl")
  val WEB_RESOURCE_PATH = config.getString("webResourcePath").getOrElse("/tmp")

  val NAVE_BULK_API_AUTH_TOKEN = configStringNoSlash("naveAuthToken")
  val USE_BULK_API = configFlag("useBulkApi")
  val LOG_BULK_API = configFlag("logBulkApi")

  val RDF_BASE_URL = configStringNoSlash("rdfBaseUrl")

  val NX_URI_PREFIX = s"$RDF_BASE_URL/resource"

  val TRIPLE_STORE_URL: Option[String] = config.getString("triple-store")

  val TRIPLE_STORE_LOG = configFlag("triple-store-log")

  Logger.info(s"Triple store logging $TRIPLE_STORE_LOG")

  val SINGLE_TRIPLE_STORE = TRIPLE_STORE_URL.isDefined

  val XSD_VALIDATION = configFlag("xsd-validation")
  System.setProperty("XSD_VALIDATION", XSD_VALIDATION.toString)

  private def tripleStore(implicit executionContext: ExecutionContext): TripleStore = TRIPLE_STORE_URL.map { tripleStoreUrl =>
    new Fuseki(tripleStoreUrl, TRIPLE_STORE_LOG)
  } getOrElse {
      throw new RuntimeException("Must have triple-store= ")
    }

  implicit val TS: TripleStore = tripleStore

  val periodicHarvest = system.actorOf(PeriodicHarvest.props(), "PeriodicHarvest")
  val harvestTicker = system.scheduler.schedule(1.minute, 1.minute, periodicHarvest, ScanForHarvests)
  val periodicSkosifyCheck = system.actorOf(PeriodicSkosifyCheck.props(), "PeriodicSkosifyCheck")
//  val skosifyTicker = system.scheduler.schedule(30.seconds, 30.seconds, periodicSkosifyCheck, ScanForWork)

  val check = Future(orgContext.sipFactory.prefixRepos.map(repo => repo.compareWithSchemasDelvingEu()))
  check.onFailure { case e: Exception => Logger.error("Failed to check schemas", e) }

  val authenticationService : AuthenticationService = AuthenticationMode.fromConfigString(config.getString(AuthenticationMode.PROPERTY_NAME)) match {
    case AuthenticationMode.MOCK => new MockAuthenticationService
    case AuthenticationMode.TS => new TsBasedAuthenticationService
  }

  val orgContext = new OrgContext(authenticationService, USER_HOME, ORG_ID)(global, tripleStore)

  def actorWork(actorContext: ActorContext)(block: => Unit) = {
    try {
      block
    }
    catch {
      case e: Throwable =>
        actorContext.parent ! WorkFailure(e.getMessage, Some(e))
    }
  }
}

class OrgContext(val authenticationService: AuthenticationService, userHome: String, val orgId: String)(implicit ec: ExecutionContext, ts: TripleStore) {

  val root = new File(userHome, "NarthexFiles")
  val orgRoot = new File(root, orgId)
  val factoryDir = new File(orgRoot, "factory")
  val categoriesDir = new File(orgRoot, "categories")
  val datasetsDir = new File(orgRoot, "datasets")
  val rawDir = new File(orgRoot, "raw")
  val sipsDir = new File(orgRoot, "sips")

  lazy val categoriesRepo = new CategoriesRepo(categoriesDir)
  lazy val sipFactory = new SipFactory(factoryDir)


  val us = new ActorStore(authenticationService)

  orgRoot.mkdirs()
  factoryDir.mkdirs()
  datasetsDir.mkdir()
  rawDir.mkdirs()
  sipsDir.mkdirs()

  def clear() = {
    clearDir(datasetsDir)
    clearDir(sipsDir)
    clearDir(rawDir)
    // todo: categories too when they are no longer defined there
  }

  def createDsInfo(owner: NXActor, spec: String, characterString: String, prefix: String) = {
    val character = DsInfo.getCharacter(characterString).get
    DsInfo.createDsInfo(owner, spec, character, prefix)
  }

  def datasetContext(spec: String): DatasetContext = withDsInfo(spec)(dsInfo => new DatasetContext(this, dsInfo))

  def vocabMappingStore(specA: String, specB: String): VocabMappingStore = {
    val futureStore = for {
      infoA <- VocabInfo.freshVocabInfo(specA)
      infoB <- VocabInfo.freshVocabInfo(specB)
    } yield (infoA, infoB) match {
        case (Some(a), Some(b)) => new VocabMappingStore(a, b)
        case _ => throw new RuntimeException(s"No vocabulary mapping found for $specA, $specB")
      }
    Await.result(futureStore, 15.seconds)
  }

  def termMappingStore(spec: String): TermMappingStore = {
    withDsInfo(spec)(dsInfo => new TermMappingStore(dsInfo))
  }

  def availableSips: Seq[AvailableSip] = sipsDir.listFiles.toSeq.filter(
    _.getName.endsWith(SIP_EXTENSION)
  ).map(AvailableSip).sortBy(_.dateTime.getMillis).reverse

  def uploadedSips: Future[Seq[Sip]] = {
    DsInfo.listDsInfo.map { list =>
      list.flatMap { dsi =>
        val datasetContext = new DatasetContext(this, dsi)
        datasetContext.sipRepo.latestSipOpt
      }
    }
  }

  def startCategoryCounts() = {
    val catDatasets = DsInfo.listDsInfo.map(_.filter(_.getBooleanProp(categoriesInclude)))
    catDatasets.map { dsList =>
      OrgActor.actor ! DatasetsCountCategories(dsList.map(_.spec))
    }
  }

}