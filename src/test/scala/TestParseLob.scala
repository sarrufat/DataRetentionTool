
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import scala.xml.XML

class TestParseLob extends FlatSpec {
  "Test XML.load" should "Load file" in {
    val root = XML.load("./src/test/resources/LobInsert.xml")
    root should not be empty
    val tables = (root \\ "lob" \\ "table")
    tables.size shouldEqual 1073
  }
}
