//===========================================================================
//    Copyright 2015 Delving B.V.
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

import java.io.{File, StringWriter}

import com.hp.hpl.jena.rdf.model._
import dataset.SourceRepo.{IdFilter, VERBATIM_FILTER}
import harvest.Harvesting.{HarvestCron, HarvestType}
import mapping.{SkosVocabulary, TermMappingStore, VocabInfo}
import net.sf.saxon.functions.False
import org.ActorStore.NXActor
import org.OrgActor.DatasetMessage
import org.OrgContext
import org.OrgContext.{NX_URI_PREFIX, orgContext}
import org.apache.jena.riot.{RDFDataMgr, RDFFormat}
import org.joda.time.DateTime
import play.api.Logger
import play.api.Play.current
import play.api.cache.Cache
import play.api.libs.json.{JsObject, JsValue, Json, Writes}
import play.api.libs.ws.{WS, WSResponse}
import services.StringHandling.{createGraphName, urlEncodeValue}
import services.Temporal._
import triplestore.GraphProperties._
import triplestore.Sparql._
import triplestore.{SkosGraph, TripleStore}

import scala.collection.JavaConversions._
import scala.concurrent._
import scala.concurrent.duration._
import scala.io.Source

object DsInfo {

  val patience = 1.minute

  val cacheTime = 10.minutes

  case class DsCharacter(name: String)

  val CharacterMapped = DsCharacter("character-mapped")

  def getCharacter(characterString: String) = List(CharacterMapped).find(_.name == characterString)

  case class DsState(prop: NXProp) {
    override def toString = prop.name
  }

  object DsState {
    val RAW = DsState(stateRaw)
    val RAW_ANALYZED = DsState(stateRawAnalyzed)
    val SOURCED = DsState(stateSourced)
    val MAPPABLE = DsState(stateMappable)
    val PROCESSABLE = DsState(stateProcessable)
    val PROCESSED = DsState(stateProcessed)
    val ANALYZED = DsState(stateAnalyzed)
    val SAVED = DsState(stateSaved)
    val INCREMENTAL_SAVED = DsState(stateIncrementalSaved)
    val SYNCED = DsState(stateSynced)
  }

  case class DsMetadata(name: String,
                        description: String,
                        aggregator: String,
                        owner: String,
                        language: String,
                        rights: String)

  implicit val dsInfoWrites = new Writes[DsInfo] {
    def writes(dsInfo: DsInfo): JsValue = {
      val out = new StringWriter()
      RDFDataMgr.write(out, dsInfo.getModel, RDFFormat.JSONLD_FLAT)
      Json.parse(out.toString)
    }
  }

  def listDsInfo(implicit ec: ExecutionContext, ts: TripleStore): Future[List[DsInfo]] = {
    ts.query(selectDatasetSpecsQ).map { list =>
      list.map { entry =>
        val spec = entry("spec").text
        new DsInfo(spec)
      }
    }
  }

  def getDsInfoUri(spec: String) = s"$NX_URI_PREFIX/dataset/${urlEncodeValue(spec)}"

  def getGraphName(spec: String) = createGraphName(getDsInfoUri(spec))

  def getSkosGraphName(datasetUri: String) = createGraphName(s"$datasetUri/skos")

  def createDsInfo(owner: NXActor, spec: String, character: DsCharacter, mapToPrefix: String)(implicit ec: ExecutionContext, ts: TripleStore): Future[DsInfo] = {
    val m = ModelFactory.createDefaultModel()
    val subject = m.getResource(getDsInfoUri(spec))
    m.add(subject, m.getProperty(rdfType), m.getResource(datasetEntity))
    m.add(subject, m.getProperty(datasetSpec.uri), m.createLiteral(spec))
    m.add(subject, m.getProperty(orgId.uri), m.createLiteral(orgContext.orgId))
    m.add(subject, m.getProperty(datasetCharacter.uri), m.createLiteral(character.name))
    m.add(subject, m.getProperty(actorOwner.uri), m.createResource(owner.uri))
    val trueLiteral = m.createLiteral("true")
    val falseLiteral = m.createLiteral("false")
    m.add(subject, m.getProperty(publishOAIPMH.uri), trueLiteral)
    m.add(subject, m.getProperty(publishIndex.uri), trueLiteral)
    m.add(subject, m.getProperty(publishLOD.uri), trueLiteral)
    if (mapToPrefix != "-") m.add(subject, m.getProperty(datasetMapToPrefix.uri), m.createLiteral(mapToPrefix))
    ts.up.dataPost(getGraphName(spec), m).map { ok =>
      val cacheName = getDsInfoUri(spec)
      val dsInfo = new DsInfo(spec)
      Cache.set(cacheName, dsInfo, cacheTime)
      dsInfo
    }
  }

  def freshDsInfo(spec: String)(implicit ec: ExecutionContext, ts: TripleStore): Future[Option[DsInfo]] = {
    ts.ask(askIfDatasetExistsQ(getDsInfoUri(spec))).map(answer =>
      if (answer) Some(new DsInfo(spec)) else None
    )
  }

  def withDsInfo[T](spec: String)(block: DsInfo => T)(implicit ec: ExecutionContext, ts: TripleStore) = {
    val cacheName = getDsInfoUri(spec)
    Cache.getAs[DsInfo](cacheName) map { dsInfo =>
      block(dsInfo)
    } getOrElse {
      val dsInfo = Await.result(freshDsInfo(spec), 30.seconds).getOrElse {
        throw new RuntimeException(s"No dataset info for $spec")
      }
      Cache.set(cacheName, dsInfo, cacheTime)
      block(dsInfo)
    }
  }
  def getDsInfo(spec: String)(implicit ec: ExecutionContext, ts: TripleStore) = {
    new DsInfo(spec)
  }
}

class DsInfo(val spec: String)(implicit ec: ExecutionContext, ts: TripleStore) extends SkosGraph {

  import dataset.DsInfo._

  def now: String = timeToString(new DateTime())

  val uri = getDsInfoUri(spec)

  val graphName = getGraphName(spec)

  val skosified = true

  val skosGraphName = getSkosGraphName(uri)

  // could cache as well so that the get happens less
  def futureModel = ts.dataGet(graphName)

  futureModel.onFailure {
    case e: Throwable => Logger.warn(s"No data found for dataset $spec", e)
  }

  def getModel = Await.result(futureModel, patience)

  def getLiteralProp(prop: NXProp): Option[String] = {
    val m = getModel
    val res = m.getResource(uri)
    val objects = m.listObjectsOfProperty(res, m.getProperty(prop.uri))
    if (objects.hasNext) Some(objects.next().asLiteral().getString) else None
  }

  def getTimeProp(prop: NXProp): Option[DateTime] = getLiteralProp(prop).map(stringToTime)

  def getBooleanProp(prop: NXProp): Boolean = getLiteralProp(prop).exists(_ == "true")

  def setSingularLiteralProps(propVals: (NXProp, String)*): Unit = {
    val sparqlPerPropQ = propVals.map(pv => updatePropertyQ(graphName, uri, pv._1, pv._2)).toList
    val withSynced = updateSyncedFalseQ(graphName, uri) :: sparqlPerPropQ
    val sparql = withSynced.mkString(";\n")
    val futureUpdate = ts.up.sparqlUpdate(sparql)
    Await.ready(futureUpdate, patience)
  }

  def removeLiteralProp(prop: NXProp): Unit = {
    val futureUpdate = ts.up.sparqlUpdate(removeLiteralPropertyQ(graphName, uri, prop))
    Await.ready(futureUpdate, patience)
  }

  def getLiteralPropList(prop: NXProp): List[String] = {
    val m = getModel
    m.listObjectsOfProperty(m.getResource(uri), m.getProperty(prop.uri)).map(_.asLiteral().toString).toList
  }

  def addLiteralPropToList(prop: NXProp, uriValueString: String): Unit = {
    val futureUpdate = ts.up.sparqlUpdate(addLiteralPropertyToListQ(graphName, uri, prop, uriValueString))
    Await.ready(futureUpdate, patience)
  }

  def removeLiteralPropFromList(prop: NXProp, uriValueString: String): Unit = {
    val futureUpdate = ts.up.sparqlUpdate(deleteLiteralPropertyFromListQ(graphName, uri, prop, uriValueString))
    Await.ready(futureUpdate, patience)
  }

  def getUriProp(prop: NXProp): Option[String] = {
    val m = getModel
    m.listObjectsOfProperty(m.getResource(uri), m.getProperty(prop.uri)).toList.headOption.map(_.asResource().toString)
  }

  def dropDataset = {
    removeNaveDataSet()
    ts.up.sparqlUpdate(deleteDatasetQ(graphName, uri, skosGraphName)).map(ok => true)
  }

  def dropDatasetRecords = {
    ts.up.sparqlUpdate(deleteDatasetRecordsQ(uri)).map(ok => true)
  }

  def dropDatasetIndex = {
    disableInNaveIndex()
    true
  }

  def getIdFilter: IdFilter = {
    getLiteralProp(idFilterType).map { filterType =>
      val expressionOpt = getLiteralProp(idFilterExpression).flatMap(ex => if (ex.trim.isEmpty) None else Some(ex))
      IdFilter(filterType, expressionOpt)
    } getOrElse {
      VERBATIM_FILTER
    }
  }

  def setState(state: DsState) = setSingularLiteralProps(state.prop -> now)

  def setHarvestIncrementalMode(enabled: Boolean = false) = {
    val mode = if (enabled) "true" else "false"
    setSingularLiteralProps(harvestIncrementalMode -> mode)
  }

  def setLastHarvestTime(incremental: Boolean = false) = {
    if (incremental) {
      // todo: fix this by setting the start date
      setSingularLiteralProps(lastIncrementalHarvestTime -> now)
      setHarvestIncrementalMode(true)
    } else {
      setSingularLiteralProps(lastFullHarvestTime -> now)
      setHarvestIncrementalMode(false)
      setIncrementalProcessedRecordCounts(0, 0)
      removeLiteralProp(lastIncrementalHarvestTime)
    }
  }

  def setRecordsSync(state: Boolean = false) = {
    val syncState = state match {
      case true => "true"
      case false => "false"
    }
    removeLiteralProp(datasetRecordsInSync)
    setSingularLiteralProps(datasetRecordsInSync -> syncState)
  }

  def setProxyResourcesSync(state: Boolean = false) = {
    val syncState = state match {
      case true => "true"
      case false => "false"
    }
    removeLiteralProp(datasetResourcePropertiesInSync)
    setSingularLiteralProps(datasetResourcePropertiesInSync -> syncState)
  }

  def removeState(state: DsState) = removeLiteralProp(state.prop)

  def setError(message: String) = {
    if (message.isEmpty) {
      removeLiteralProp(datasetErrorMessage)
    }
    else {
      setSingularLiteralProps(
        datasetErrorMessage -> message,
        datasetErrorTime -> now
      )
    }
  }

  val LineId = "<!--<([^>]+)__([^>]+)>-->".r

  def uniqueCounterSource(sourceDirectory: String): Map[String, Int] = {
    val sourceFiles = new File(sourceDirectory)
    if (!sourceFiles.exists()){
      return Map.empty[String, Int]
    }
    sourceFiles
      .listFiles
      .filter(_.getName.endsWith(".ids"))
      .flatMap(Source.fromFile(_).getLines)
      .foldLeft(Map.empty[String, Int]){
        (count, keyword) => {
          count + (keyword -> (count.getOrElse(keyword, 0) + 1))
        }
      }
  }

  def uniqueCounterProcessed(processedRecordDiretory: String): Map[String, Int] = {
    val processedFiles = new File(processedRecordDiretory)
    if (!processedFiles.exists()){
      return Map.empty[String, Int]
    }
    processedFiles
      .listFiles
      .filter(_.getName.endsWith(".xml"))
      .flatMap{
        fname => {
          Source.fromFile(fname).getLines.filter{line =>
            line match {
              case LineId(graphName, currentHash) => true
              case _ => false
            }}.map{line =>
            line match {
              case LineId(graphName, currentHash) =>  graphName
              case _ =>
            }
          }
        }
      }
      .foldLeft(Map.empty[String, Int]){
        (count, word) =>
          val keyword = word.asInstanceOf[String]
          count + (keyword -> (count.getOrElse(keyword, 0) + 1))
      }
  }

  def updatedSpecCountFromFile(
                                   specName: String = spec,
                                   narthexBaseDir: File = OrgContext.NARTHEX,
                                   orgId: String = OrgContext.ORG_ID): (Int, Int, Int) = {

    val spec_source_dir = s"""${narthexBaseDir.toString}/$orgId/datasets/$specName"""
    val processed_dir = s"$spec_source_dir/processed"
    val source_dir = s"$spec_source_dir/source"
    val valid: Int = uniqueCounterProcessed(processed_dir).size
    val total: Int = uniqueCounterSource(source_dir).size
    val invalid = total - valid
    setRecordCount(total)
    setProcessedRecordCounts(valid, invalid)
    (invalid, valid, total)
  }

  def setRecordCount(count: Int) = setSingularLiteralProps(datasetRecordCount -> count.toString)

  def setProcessedRecordCounts(validCount: Int, invalidCount: Int) = setSingularLiteralProps(
    processedValid -> validCount.toString,
    processedInvalid -> invalidCount.toString
  )

  def setIncrementalProcessedRecordCounts(validCount: Int, invalidCount: Int) = setSingularLiteralProps(
    processedIncrementalValid -> validCount.toString,
    processedIncrementalInvalid -> invalidCount.toString
  )

  def setHarvestInfo(harvestTypeEnum: HarvestType, url: String, dataset: String, prefix: String) = setSingularLiteralProps(
    harvestType -> harvestTypeEnum.name,
    harvestURL -> url,
    harvestDataset -> dataset,
    harvestPrefix -> prefix
  )

  def setHarvestCron(harvestCron: HarvestCron = currentHarvestCron) = setSingularLiteralProps(
    harvestPreviousTime -> timeToLocalString(harvestCron.previous),
    harvestDelay -> harvestCron.delay.toString,
    harvestDelayUnit -> harvestCron.unit.toString
  )

  def setMetadata(metadata: DsMetadata) = setSingularLiteralProps(
    datasetName -> metadata.name,
    datasetDescription -> metadata.description,
    datasetAggregator -> metadata.aggregator,
    datasetOwner -> metadata.owner,
    datasetLanguage -> metadata.language,
    datasetRights -> metadata.rights
  )

  def toggleNaveSkosField(datasetUri: String, propertyUri: String, delete: Boolean = false) = {
    def checkUpdateResponse(response: WSResponse, logString: JsObject): Unit = {
      if (response.status != 201) {
        Logger.error(logString.toString())
        throw new Exception(s"${response.statusText}: ${response.body}:")
      }
    }

    val skosFieldApi = s"${OrgContext.NAVE_API_URL}/api/index/narthex/toggle/proxyfield/"
    val request = WS.url(s"$skosFieldApi").withHeaders(
      "Content-Type" -> "application/json; charset=utf-8",
      "Accept" -> "application/json",
      "Authorization" -> s"Token ${OrgContext.NAVE_BULK_API_AUTH_TOKEN}"
    )
    val json = Json.obj(
      "dataset_uri" -> datasetUri,
      "property_uri" -> propertyUri,
      "delete" -> delete
    )
    request.post(json) // .map(checkUpdateResponse(_, json))
  }


  def currentHarvestCron = {
    (getLiteralProp(harvestPreviousTime), getLiteralProp(harvestDelay), getLiteralProp(harvestDelayUnit), getLiteralProp(harvestIncremental)) match {
      case (Some(previousString), Some(delayString), Some(unitString), Some(incrementalString)) =>
        val previousTime = try {
          stringToTime(previousString)
        } catch {
          case iae: IllegalArgumentException =>
            stringToTime("1970-01-01")
        }
        val delay = try {
            delayString.toInt
        } catch {
          case ine: NumberFormatException =>
            0
        }
        if (delay == 0) {
          HarvestCron(new DateTime(), 1, DelayUnit.WEEKS, incremental = false)
        } else {
          HarvestCron(
            previous = previousTime,
            delay = delay,
            unit = DelayUnit.fromString(unitString).getOrElse(DelayUnit.WEEKS),
            incremental = incrementalString.toBoolean
          )
        }
      case _ =>
        HarvestCron(new DateTime(), 1, DelayUnit.WEEKS, incremental = false)
    }
  }

  def termCategoryMap(categoryVocabularyInfo: VocabInfo): Map[String, List[String]] = {
    val mappingStore = new TermMappingStore(this)
    val mappings = Await.result(mappingStore.getMappings(categories = true), 1.minute)
    val uriLabelMap = categoryVocabularyInfo.vocabulary.uriLabelMap
    val termUriLabels = mappings.flatMap { mapping =>
      val termUri = mapping.head
      val categoryUri = mapping(1)
      uriLabelMap.get(categoryUri).map(label => (termUri, label))
    }
    termUriLabels.groupBy(_._1).map(group => group._1 -> group._2.map(_._2))
  }

  // for actors

  def createMessage(payload: AnyRef) = DatasetMessage(spec, payload)

  def toTurtle = {
    val sw = new StringWriter()
    getModel.write(sw, "TURTLE")
    sw.toString
  }

  lazy val vocabulary = new SkosVocabulary(spec, skosGraphName)

  def ownerEmailOpt = getUriProp(actorOwner).flatMap(ownerUri => orgContext.us.emailFromUri(ownerUri))

  def orUnknown(nxProp: NXProp) = getLiteralProp(nxProp).getOrElse("Unknown")

  def processedValidVal = orUnknown(processedValid)

  def processedInvalidVal = orUnknown(processedInvalid)

  override def toString = spec

  val bulkApi = s"${OrgContext.NAVE_API_URL}/api/index/bulk/"

  private def checkUpdateResponse(response: WSResponse, logString: String): Unit = {
    if (response.status != 201) {
      Logger.error(logString)
      throw new Exception(s"${response.statusText}: ${response.body}:")
    }
  }

  def bulkApiUpdate(bulkActions: String) = {
    if (OrgContext.LOG_BULK_API) {
      Logger.info(bulkActions)
    }
    val request = WS.url(s"$bulkApi").withHeaders(
      "Content-Type" -> "text/plain; charset=utf-8",
      "Accept" -> "application/json",
      "Authorization" -> s"Token ${OrgContext.NAVE_BULK_API_AUTH_TOKEN}"
    )
    request.post(bulkActions).map(checkUpdateResponse(_, bulkActions))
  }

  def extractSpecIdFromGraphName(id: String): (String, String) = {
    val SpecIdExtractor = "http://.*?/resource/aggregation/([^/]+)/([^/]+)/graph".r
    val SpecIdExtractor(spec, localId) = id
    (spec, localId)
  }

  def removeNaveOrphans(timeStamp: String) = {
    val actionMap = Json.obj(
      "dataset" -> spec,
      // "acceptanceMode" -> acceptanceModeString,
      "action" -> "clear_orphans",
      "modification_date" -> timeStamp
    )
    bulkApiUpdate(s"${actionMap.toString()}\n")
  }

  def disableInNaveIndex() = {
    val actionMap = Json.obj(
      "dataset" -> spec,
      // "acceptanceMode" -> acceptanceModeString,
      "action" -> "disable_index"
    )
    bulkApiUpdate(s"${actionMap.toString()}\n")
  }

  def removeNaveDataSet() = {
    val actionMap = Json.obj(
      "dataset" -> spec,
      // "acceptanceMode" -> acceptanceModeString,
      "action" -> "drop_dataset"
    )
    bulkApiUpdate(s"${actionMap.toString()}\n")
  }



  def createBulkAction(triples: String, id: String, hash: String) = {
    val (spec, localId) = extractSpecIdFromGraphName(id)
    val hubId = s"${OrgContext.ORG_ID}_${spec}_$localId"
    // val currentSkosFields = dsInfo.getLiteralPropList(skosField)
    val acceptanceModeString= "false"
    val actionMap = Json.obj(
      "hubId" -> hubId,
      "dataset" -> spec,
      // "acceptanceMode" -> acceptanceModeString,
      "graphUri" -> id,
      "type" -> "void_EDMRecord",
      "action" -> "index",
      "contentHash" -> hash,
      "graph" -> s"$triples".stripMargin.trim
    )
    s"${actionMap.toString()}\n"
  }

}
