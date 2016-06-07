
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.matchers._
import curam.util.oracle.sql.parser.SQLParser
import curam.util.oracle.sql.parser.Statement
import curam.util.oracle.sql.diff.MemoryDB
import java.io.FileWriter
import java.io.BufferedWriter
import java.util.Comparator

class MemoryDBTest extends FlatSpec {

  "MemoryDB " should "past all tests" in {
    val parser = new SQLParser
    val rcurrent = parser.parse("./src/test/resources/CTSource.sql")
    rcurrent should not be empty

    val rtarget = parser.parse("./src/test/resources/CTTarget.sql")
    rtarget should not be empty
    val mem = MemoryDB(rcurrent.get)
    val writer = new BufferedWriter(new FileWriter("testMemory.sql"))
    val exludeTabs = Seq("KEYSERVER", "PRODUCTPROVIDER")
    val exludedDiffFields = Seq("LASTWRITTEN")
    val diff = mem.diffAndWrite(rtarget.get, Option(MemoryDB.ExcludeOption(exludeTabs, exludedDiffFields)), writer)
    diff should not be empty

  }
}
