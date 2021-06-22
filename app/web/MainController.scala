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

package web

import java.io.File

import controllers.WebJarAssets
import org.webjars.play.RequireJS
import play.api.cache.{CacheApi, Cached}
import play.api.mvc._

import scala.concurrent.duration._


class MainController(val cacheApi: CacheApi,
                     val narthexDomain: String, val naveDomain: String, val orgId: String,
                     val webJarAssets: WebJarAssets, val requireJS: RequireJS,
                     val sipAppDownloadUrl: String, val enableIncrementalHarvest: Boolean,
                     val supportedDatasetTypes: List[String])
  extends Controller {

  val cacheDuration = 1.day

  /**
    * Caching action that caches an OK response for the given amount of time with the key.
    * NotFound will be cached for 5 mins. Any other status will not be cached.
    */
  def Caching(key: String, okDuration: Duration) = {
    new Cached(cacheApi)
      .status(_ => key, OK, okDuration.toSeconds.toInt)
      .includeStatus(NOT_FOUND, 5.minutes.toSeconds.toInt)
  }

  def root = Action { request =>
    Redirect("/narthex/")
  }

  def index = Action { request =>
    Ok(views.html.index(orgId, sipAppDownloadUrl, buildinfo.BuildInfo.version,
      supportedDatasetTypes.mkString(","),
      buildinfo.BuildInfo.gitCommitSha, webJarAssets, requireJS, naveDomain, enableIncrementalHarvest)).withHeaders(
      CACHE_CONTROL -> "no-cache")
  }

  /**
    * In actual deployment scenarios this will result in a view on the page that provides links to Nave,
    * nave-search and narthex itself.
    */
  def logout = Action {
    Unauthorized(views.html.logout())
  }

  def OkFile(file: File, attempt: Int = 0): Result = Utils.okFile(file, attempt)

  def OkXml(xml: String): Result = Utils.okXml(xml)

  /**
    * Retrieves all routes via reflection.
    * http://stackoverflow.com/questions/12012703/less-verbose-way-of-generating-play-2s-javascript-router
    * @todo If you have controllers in multiple packages, you need to add each package here.
    */
  val routeCache = {
    val jsRoutesClasses = Seq(classOf[routes.javascript]) // TODO add your own packages
    jsRoutesClasses.flatMap { jsRoutesClass =>
      val controllers = jsRoutesClass.getFields.map(_.get(null))
      controllers.flatMap { controller =>
        controller.getClass.getDeclaredMethods.filter(_.getName != "_defaultPrefix").map { action =>
          action.invoke(controller).asInstanceOf[play.api.routing.JavaScriptReverseRoute]
        }
      }
    }
  }

  /**
    * Returns the JavaScript router that the client can use for "type-safe" routes.
    * Uses browser caching; set duration (in seconds) according to your release cycle.
    * @param varName The name of the global variable, defaults to `jsRoutes`
    */
  def jsRoutes(varName: String = "jsRoutes") = Caching("jsRoutes", cacheDuration) {
    Action { implicit request =>
      Ok(play.api.routing.JavaScriptReverseRouter(varName)(routeCache: _*)).as(JAVASCRIPT)
    }
  }

}
