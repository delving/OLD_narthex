package specs

import java.io.{File, FileOutputStream}

import mapping.CategoryDb
import org.apache.poi.xssf.usermodel._
import org.scalatest.{FlatSpec, Matchers}
import record.CategoryParser
import services.{FileHandling, ProgressReporter}

class TestCategoryParser extends FlatSpec with Matchers {

  val categoryMappingSource =
    <category-mappings>
      <category-mapping>
        <source>dimcon/Martena__test/input/dc:subject/Franeker</source>
        <categories>
          <schi/>
        </categories>
      </category-mapping>
      <category-mapping>
        <source>dimcon/Martena__test/input/dc:subject/Martinikerk</source>
        <categories>
          <tekn/>
        </categories>
      </category-mapping>
      <category-mapping>
        <source>dimcon/Martena__test/input/dc:subject/bolwerken</source>
        <categories>
          <grfk/>
          <kemk/>
        </categories>
      </category-mapping>
      <category-mapping>
        <source>dimcon/Martena__test/input/dc:subject/Westra-Bakker%2C%20M.</source>
        <categories>
          <bldh/>
        </categories>
      </category-mapping>
    </category-mappings>

  val categoryMappingList = CategoryDb.getList(categoryMappingSource)

  "category parser" should "count the occurrences of single, paired, and triple categories in records" in {
    val url = getClass.getResource("/Martena.xml.gz")
    val file = new File(url.getFile)
    val (source, readProgress) = FileHandling.sourceFromFile(file)
    val recordRoot = "/delving-sip-source/input"
    val uniqueId = s"$recordRoot/@id"
    val pathPrefix = "dimcon/Martena__test"
    val categoryMappings = categoryMappingList.map(cm => (cm.source, cm)).toMap
    val categoryParser = new CategoryParser(pathPrefix, recordRoot, uniqueId, None, categoryMappings)
    case class Counter(var count: Int)
    val reply = categoryParser.parse(source, Set.empty[String], ProgressReporter())
    val countMap = categoryParser.categoryCounts
    println(s"cat map $countMap")
    reply should be(true)
    categoryParser.recordCount should be(4724)
    val expected =
      "Map(bldh -> 13, kemk -> 16, schi:tekn -> 23, grfk:kemk:schi -> 16, kemk:schi -> 16," +
        " grfk:kemk -> 16, schi -> 280, tekn -> 24, grfk:schi -> 16, grfk -> 16)"
    countMap.toString() should be(expected)

    val wb = new XSSFWorkbook
    val sheet = wb.createSheet("Categories")
    var rowNumber = 0
    countMap.map { entry =>
      var row = sheet.createRow(rowNumber)
      row.createCell(0).setCellValue(entry._1)
      row.createCell(1).setCellValue(entry._2)
      rowNumber += 1
    }

    def createSpreadsheet() = {
      val home = new File(System.getProperty("user.home"));
      val excel = new File(home, "categories.xlsx")
      val fos = new FileOutputStream(excel)
      wb.write(fos)
      fos.close()
    }


  }
}