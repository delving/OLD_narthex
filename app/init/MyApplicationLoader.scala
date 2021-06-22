package init

import java.io.File
import java.util
import java.util.concurrent.TimeUnit

import akka.actor.{ActorRef, Props}
import com.codahale.metrics.MetricRegistry
import com.codahale.metrics.health.HealthCheckRegistry
import com.kenshoo.play.metrics.{MetricsController, MetricsFilterImpl, MetricsImpl}
import controllers.WebJarAssets
import harvest.PeriodicHarvest
import harvest.PeriodicHarvest.ScanForHarvests
import init.MyApplicationLoader.DatadogReportingConfig
import init.healthchecks.FusekiHealthCheck
import mapping.PeriodicSkosifyCheck
import org.coursera.metrics.datadog.DatadogReporter
import org.coursera.metrics.datadog.transport.UdpTransport
import org.webjars.play.RequireJS
import organization._
import play.api.ApplicationLoader.Context
import play.api._
import play.api.cache.EhCacheComponents
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.mailer._
import play.api.libs.ws.ahc.AhcWSComponents
import router.Routes
import services.{MailService, PlayMailService}
import triplestore.Fuseki
import web._

import scala.collection.JavaConverters._
import scala.concurrent.Future
import scala.concurrent.duration._ // this is the class that Play generates based on the contents of the Routes file

object MyApplicationLoader {
  val topActorConfigProp = "topActor.initialPassword"
  val datadogEnabledConfigProp = "datadog.enabled"
  val datadogStatsdPortConfigProp = "datadog.statsdPort"
  val datadogStatsDHostConfigProp = "datadog.statsdHost"
  val datadogIntervalConfigProp = "datadog.reportingIntervalInSeconds"

  case class DatadogReportingConfig(intervalInSeconds: Long, statsdHost: String, statsdPort: Int)
}


class MyApplicationLoader extends ApplicationLoader {

  import MyApplicationLoader._

  def load(context: Context) = {
    Logger.info("Narthex starting up...")

    val homeDir: String = context.initialConfiguration.getString("narthexHome").
      getOrElse(System.getProperty("user.home") + "/NarthexFiles")

    val narthexDataDir: File = new File(homeDir)
    if (! narthexDataDir.canWrite ) {
      throw new RuntimeException(s"Configured $narthexDataDir is not writeable")
    }

    val datadogEnabled = context.initialConfiguration.getBoolean(MyApplicationLoader.datadogEnabledConfigProp)
      .getOrElse(throw new RuntimeException(s"Mandatory configprop ${MyApplicationLoader.datadogEnabledConfigProp} not set"))

    val datadogConfigOpt: Option[DatadogReportingConfig] =
      if (datadogEnabled) {
        val interval = context.initialConfiguration.getLong(MyApplicationLoader.datadogIntervalConfigProp).getOrElse(
          throw new RuntimeException(s"Mandatory configprop ${MyApplicationLoader.datadogIntervalConfigProp} not set"))

        val host = context.initialConfiguration.getString(MyApplicationLoader.datadogStatsDHostConfigProp)
          .getOrElse("localhost")
        val port = context.initialConfiguration.getInt(MyApplicationLoader.datadogStatsdPortConfigProp).getOrElse(8125)
        Some(DatadogReportingConfig(interval, host,  port))
      } else {
        None
      }

    val components = new MyComponents(context, narthexDataDir, datadogConfigOpt)
    components.application
  }
}

class MyComponents(context: Context, narthexDataDir: File, datadogConfig: Option[DatadogReportingConfig])
  extends BuiltInComponentsFromContext(context)
  with AhcWSComponents
  with EhCacheComponents
  with MailerComponents {

  LoggerConfigurator(context.environment.classLoader).foreach { _.configure(context.environment) }

  val allSupportedDatasetTypes = Set("narthex","rdf","nt","photo","ead")
  val appConfig = initAppConfig(narthexDataDir)

  val tripleStoreUrl = configStringNoSlash("triple-store")
  val tripleStoreLog = configFlag("triple-store-log")
  val sparqlQueryPath = configStringNoSlash("sparql-query-path")
  val sparqlUpdatePath = configStringNoSlash("sparql-update-path")
  val graphStorePath = configStringNoSlash("graph-store-path")
  val graphStoreParam = configStringNoSlash("graph-store-param")

  implicit val tripleStore = new Fuseki(tripleStoreUrl, appConfig.orgId, sparqlQueryPath, sparqlUpdatePath, graphStorePath, graphStoreParam, tripleStoreLog, wsApi)

  lazy val orgContext = new OrgContext(appConfig, defaultCacheApi, wsApi, mailService, orgActorRef)

  lazy val router = new Routes(httpErrorHandler, mainController, webSocketController, appController,
    sipAppController, apiController, webJarAssets, assets, metricsController, infoController)

  lazy val orgActorRef: ActorRef = actorSystem.actorOf(Props(new OrgActor(orgContext, harvestingExecutionContext)), appConfig.orgId)
  val harvestingExecutionContext = actorSystem.dispatchers.lookup("contexts.dataset-harvesting-execution-context")
  val metrics = new MetricsImpl(applicationLifecycle, configuration)

  lazy val healthCheckRegistry = new HealthCheckRegistry

  val reporterOpt = initReporter(datadogConfig, metrics.defaultRegistry)
  lazy val metricsFilter = new MetricsFilterImpl(metrics)

  val sessionTimeoutInSeconds: Int = configuration.getInt("sessionTimeoutInSeconds").getOrElse(60 * 60 * 4)

  override lazy val httpFilters = List(metricsFilter)
  lazy val requireJs = new RequireJS(environment, webJarAssets)
  lazy val metricsController = new MetricsController(metrics)
  lazy val sipAppController: SipAppController = new SipAppController(orgContext)
  lazy val mainController = new MainController(defaultCacheApi,
    appConfig.narthexDomain, appConfig.naveDomain, appConfig.orgId,
    webJarAssets, requireJs, configString("sipAppDownloadUrl"), configFlag("enableIncrementalHarvest"),
    appConfig.supportedDatasetTypes)

  lazy val appController = new AppController(orgContext) (tripleStore, actorSystem, materializer)
  lazy val apiController = new APIController(orgContext)
  lazy val infoController = new InfoController(healthCheckRegistry)

  lazy val webSocketController = new WebSocketController() (actorSystem, materializer, defaultContext)

  lazy val assets = new controllers.Assets(httpErrorHandler)
  lazy val webJarAssets = new WebJarAssets(httpErrorHandler, configuration, environment)

  val periodicHarvest = actorSystem.actorOf(PeriodicHarvest.props(orgContext), "PeriodicHarvest")
  val harvestTicker = actorSystem.scheduler.schedule(1.minute, 1.minute, periodicHarvest, ScanForHarvests)
  val periodicSkosifyCheck = actorSystem.actorOf(PeriodicSkosifyCheck.props(orgContext), "PeriodicSkosifyCheck")

  //val check = Future(orgContext.sipFactory.prefixRepos.map(repo => repo.compareWithSchemasDelvingEu()))
  //check.onFailure { case e: Exception => throw new RuntimeException(e) }

  val xsdValidation = configFlag("xsd-validation")
  System.setProperty("XSD_VALIDATION", xsdValidation.toString)

  lazy val emailReportsTo: List[String] = configuration.getStringList("emailReportsTo")
    .map(_.asScala.toList).getOrElse(List())

  lazy val mailService: MailService = new PlayMailService(mailerClient, emailReportsTo)

  //healthCheckRegistry.register("fuseki", new FusekiHealthCheck(tripleStore, configLong("healthchecks.fuseki.timeoutMillis")))

  applicationLifecycle.addStopHook { () =>
    Future.successful({
      Logger.info("Narthex shutting down, cleaning up active threads...")
      harvestTicker.cancel()
    })

  }

  private def initReporter(configOpt: Option[DatadogReportingConfig], registry: MetricRegistry): Option[DatadogReporter] = {
    configOpt match {
      case None => None
      case Some(config) => {
        val tags = List("narthex", s"v${buildinfo.BuildInfo.version}", s"sha-${buildinfo.BuildInfo.gitCommitSha}")

        val transport = new UdpTransport.Builder()
          .withPort(config.statsdPort)
          .withStatsdHost(config.statsdHost)
          .build()

        val reporter = DatadogReporter.forRegistry(registry)
          .withTransport(transport)
          .withTags(tags.asJava)
          .withPrefix("narthex")
          .build()
        reporter.start(config.intervalInSeconds, TimeUnit.SECONDS)
        Logger.info(s"Started Datadog reporter, reporting interval: ${config.intervalInSeconds} seconds")
        Some(reporter)
      }
    }

  }

  private def initAppConfig(narthexDataDir: File): AppConfig = {
    val harvestTimeout = configuration.getLong("harvest.timeout").getOrElse(3l * 60 * 1000)
    val rdfBaseUrl = configStringNoSlash("rdfBaseUrl")
    val narthexDomain = configStringNoSlash("domains.narthex")
    val naveDomain = configStringNoSlash("domains.nave")
    val enableIncrementalHarvest = configuration.getBoolean("enableIncrementalHarvest").getOrElse(false)
    val crunchWhiteSpace = configuration.getBoolean("crunchWhiteSpace").getOrElse(true)
    val concurrencyLimit = configuration.getInt("concurrencyLimit").getOrElse(3)

    val datasetTypes: List[String] = configuration.getStringList("datasetTypes").map(_.asScala.toList).getOrElse(List())
    datasetTypes.foreach(datasetType => {
      if(!allSupportedDatasetTypes(datasetType)) {
        throw new IllegalStateException(s"Unsupported dataset type: $datasetType")
      }
    })

    AppConfig(
      harvestTimeout, true, rdfBaseUrl,
      configStringNoSlash("naveApiUrl"), configStringNoSlash("naveAuthToken"),
      configuration.getBoolean("mockBulkApi").getOrElse(false),
      narthexDataDir, configString("orgId"), narthexDomain, naveDomain, enableIncrementalHarvest,
      crunchWhiteSpace, concurrencyLimit, datasetTypes)
  }

  private def configFlag(name: String): Boolean = configuration.getBoolean(name).getOrElse(false)

  private def configString(name: String) = configuration.getString(name).
    getOrElse(throw new RuntimeException(s"Missing config string: $name"))

  private def configLong(name: String) = configuration.getLong(name).
    getOrElse(throw new RuntimeException(s"Missing/invalid config string: $name"))

  private def configStringNoSlash(name: String) = configString(name).replaceAll("\\/$", "")

}

/**
  * Contains all application-specific configuration-values obtained from the play environment and that are used
  * by various components
  */
case class AppConfig(harvestTimeOut: Long, useBulkApi: Boolean, rdfBaseUrl: String,
                     naveApiUrl: String, naveApiAuthToken: String, mockBulkApi: Boolean,
                     narthexDataDir: File, orgId: String,
                     narthexDomain: String, naveDomain: String, enableIncrementalHarvest: Boolean,
                     crunchWhiteSpace: Boolean, concurrencyLimit: Int, supportedDatasetTypes: List[String]) {

  def nxUriPrefix: String = s"$rdfBaseUrl/resource"
}
