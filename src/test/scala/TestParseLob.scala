
import org.scalatest.FlatSpec
import org.scalatest.GivenWhenThen
import org.scalatest.Matchers._
import scala.xml.XML
import curam.util.lobs.LobInsertWrappFact

class TestParseLob extends FlatSpec with GivenWhenThen {
  "Test XML.load" should "Load file" in {
    val root = XML.load("./src/test/resources/LobInsert.xml")
    root should not be empty
    val tables = (root \\ "lob" \\ "table")
    tables.size shouldEqual 1073
    val appresources = tables.filter { node ⇒ (node \ "@tablename").text == "APPRESOURCE" }
    val liws = tables.map { t ⇒ LobInsertWrappFact(t) }
    appresources.size shouldEqual 587
    liws.filter { t ⇒ t.table == "APPRESOURCE" }.size shouldEqual appresources.size
    appresources.forall { appres ⇒
      val parameters = (appres \\ "parameter")
      !parameters.isEmpty && parameters.size > 8
    } shouldBe true
    liws.forall { lob ⇒ lob.locator != "" || lob.table == "EVIDENCETYPEVERSIONDEF" } shouldBe true
  }
  "Test Digest" should "Work fine" in {
    Given("./src/test/resources/CTTarget.sql")
    val digStr = curam.util.lobs.Digest.digestFile("./src/test/resources/CTTarget.sql")
    digStr.length should be > 16
    Then(s"digStr = $digStr")
  }
}
