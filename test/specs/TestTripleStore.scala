package specs

import java.io.File

import dataset.DsInfo._
import dataset.{DsInfo, ProcessedRepo}
import org.ActorStore
import org.scalatestplus.play._
import play.api.libs.json.Json
import play.api.test.Helpers._
import services.ProgressReporter
import triplestore.GraphProperties._

class TestTripleStore extends PlaySpec with OneAppPerSuite with FakeTripleStore {

  "The processed repo should deliver sparql update chunks " in {
    cleanStart()
    val admin = await(new ActorStore(ts).authenticate("gumby", "secret gumby")).get
    val info = await(DsInfo.createDsInfo(admin, "gumby-set", CharacterMapped, "gfx", ts))
    val home = new File(getClass.getResource(s"/processed").getFile)
    val repo = new ProcessedRepo(home, info)
    val reader = repo.createGraphReader(None, ProgressReporter())
    val chunk = reader.readChunk.get
    reader.close()
    val sparql = chunk.toSparqlUpdate
    await(ts.up.sparqlUpdate(sparql))
    countGraphs must be(14)
  }

  "The dataset info object should be able to interact with the store" in {
    cleanStart()
    val admin = await(new ActorStore(ts).authenticate("gumby", "secret gumby")).get
    val dsInfo = await(DsInfo.createDsInfo(admin, "gumby-set", CharacterMapped, "gfx", ts))
    dsInfo.getLiteralProp(datasetMapToPrefix) must be(Some("gfx"))
    val model = await(dsInfo.setSingularLiteralProps(
      datasetMapToPrefix -> "pfx",
      datasetLanguage -> "nl"
    ))
    model.size() must be(9)
    dsInfo.getLiteralProp(datasetMapToPrefix) must be(Some("pfx"))
    await(dsInfo.removeLiteralProp(datasetMapToPrefix))
    dsInfo.getLiteralProp(datasetMapToPrefix) must be(None)
    model.size() must be(8)

    await(dsInfo.setSingularLiteralProps(datasetMapToPrefix -> "pfx2"))

    // uri prop
    dsInfo.getUriPropValueList(skosField) must be(List.empty)
    await(dsInfo.addUriProp(skosField, "http://purl.org/dc/elements/1.1/type"))
    dsInfo.getUriPropValueList(skosField) must be(List("http://purl.org/dc/elements/1.1/type"))
    await(dsInfo.addUriProp(skosField, "http://purl.org/dc/elements/1.1/creator"))

    def testTwo(di: DsInfo) = {
      val two = di.getUriPropValueList(skosField)
      two.size must be(2)
      two.contains("http://purl.org/dc/elements/1.1/type") must be(true)
      two.contains("http://purl.org/dc/elements/1.1/creator") must be(true)
    }

    // only tests the contained model
    testTwo(dsInfo)

    // a fresh one that has to fetch anew
    val fresh: DsInfo = await(DsInfo.freshDsInfo("gumby-set", ts)).get

    fresh.getLiteralProp(datasetMapToPrefix) must be(Some("pfx2"))
    testTwo(fresh)

    //    println(Json.prettyPrint(dsInfoWrites.writes(fresh)))

    val second = await(DsInfo.createDsInfo(admin, "pokey-set", CharacterMapped, "", ts))

    val infoList = await(listDsInfo(ts))

    infoList.foreach { info =>
      println(Json.prettyPrint(dsInfoWrites.writes(info)))
    }
  }

}
