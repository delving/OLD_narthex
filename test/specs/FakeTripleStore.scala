package specs

import java.util.concurrent.Executors

import play.api.test.Helpers._
import triplestore.GraphProperties.deleted
import triplestore.TripleStore

import scala.concurrent.ExecutionContext

trait FakeTripleStore {

  val WRONG_URI = "http://example/update-base"

  implicit val ec = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())

  // logQueries = true if you want to see them
  implicit val ts = TripleStore.single("http://localhost:3030/test", logQueries = false)

  def cleanStart() = {
    await(ts.up.sparqlUpdate("DROP ALL"))
    if (countGraphs > 0) throw new RuntimeException
  }

  def countGraphs = {
    val graphs = await(ts.query(s"SELECT DISTINCT ?g WHERE { GRAPH ?g { ?s ?p ?o } }")).map(m => m("g"))
    graphs.foreach { qv =>
      if (qv.text.contains(WRONG_URI)) throw new RuntimeException(s"Wrong URI! ${qv.text}")
    }
    graphs.size
  }

  def countDeletedGraphs = {
    val graphs = await(ts.query(s"SELECT DISTINCT ?g WHERE { GRAPH ?g { ?s <$deleted> ?o } }")).map(m => m("g"))
    graphs.size
  }

}
