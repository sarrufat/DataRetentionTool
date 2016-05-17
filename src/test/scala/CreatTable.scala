
import org.scalatest.FlatSpec
import curam.util.oracle.sql.parser.SQLParser

object CreatTable {
  val c1 = """
CREATE TABLE CREOLEATTRIBUTEINHERITANCE(
CREOLEATTRIBUTEINHERITANCEID NUMBER(19,0) not null,
ANCESTORATTRAVAILABILITYID NUMBER(19,0) not null,
DESCENDANTATTRAVAILABILITYID NUMBER(19,0) not null,
MIGRATIONACTION VARCHAR(10)
);
  """
}

class CreatTable extends FlatSpec {

  "CreateTable" should "return result" in {
    val parser = new SQLParser
    val r = parser.parse(CreatTable.c1)
    assert(!r.isEmpty)
  }
}
