
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import curam.util.oracle.sql.parser.SQLParser

class SetupDatabaseTest extends FlatSpec {
  val parser = new SQLParser

  "Parser SetupDatabase.sql" should "parse all lines" in {
    val allStmts = parser.parse("./src/test/resources/SetupDatabase.sql")
    allStmts should not be empty
  }
}
