
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.matchers._
import org.scalatest.Inspectors._
import curam.util.oracle.sql.parser.SQLParser
import curam.util.oracle.sql.parser.CreateStmt
import curam.util.oracle.sql.parser.Statement
import scala.util.parsing.input.StreamReader
import scala.util.parsing.input.PagedSeqReader
import curam.util.oracle.sql.parser.Comparator
import org.scalatest.GivenWhenThen
import curam.util.oracle.sql.diff.SQLDiff

class CreatTable extends FlatSpec with GivenWhenThen {
  "Test create big file" should "past all tests" in {
    val parser = new SQLParser
    val rcurrent = parser.parse("./src/test/resources/CTSourceClean.sql")
    rcurrent should not be empty
    rcurrent.foreach { stmts ⇒
      val crestmt = Statement.filter[CreateStmt](stmts)
      crestmt should not be empty
      crestmt.size should be > 1000
    }
    val rtarget = parser.parse("./src/test/resources/CTTargetClean.sql")
    rtarget should not be empty
    Statement.findTable(rtarget, "BSFSUBMITROSTERTAB") shouldBe defined
    Given("two rsult parsers")
    val creDiff = Comparator.findNewTables(rcurrent, rtarget)
    When("findNewTables")
    creDiff should not be empty
    Then(SQLDiff.emitNew(creDiff).mkString)
    When("findAlterTables")
    val alterDiff = Comparator.findAlterTables(rcurrent, rtarget)
    alterDiff should not be empty
    Then(SQLDiff.emitALters(alterDiff) mkString)
    When("findAlterTabsDiff")
    val alterTabsDiff = Comparator.findAlterTabsDiff(rcurrent, rtarget)
    Then(SQLDiff.emitALtersTabs(alterTabsDiff) mkString)

  }

}
