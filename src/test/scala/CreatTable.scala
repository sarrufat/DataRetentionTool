
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
import java.io.File

class CreatTable extends FlatSpec with GivenWhenThen {
  def getListOfFiles(dir: String): List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }
  "Test create big file" should "past all tests" in {
    val parser = new SQLParser
    val rcurrent = parser.parse("./src/test/resources/CTSource.sql")
    rcurrent should not be empty
    rcurrent.foreach { stmts ⇒
      val crestmt = Statement.filter[CreateStmt](stmts)
      crestmt should not be empty
      crestmt.size should be > 1000
    }
    val rtarget = parser.parse("./src/test/resources/CTTarget.sql")
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
    When("findCreateIndexDiff")
    val ctidxDiff = Comparator.findCreateIndexDiff(rcurrent, rtarget)
    Then(SQLDiff.emitCreIdx(ctidxDiff) mkString)
  }
  "Test INSERTS" should "past all tests" in {
    val CTFiles = getListOfFiles("./src/test/resources/codetable")
    CTFiles.foreach { file ⇒
      When(file.getName)
      val parser = new SQLParser
      val inserts = parser.parse(file.getPath)
      inserts should not be empty
    }

  }

}
