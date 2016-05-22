
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

object CreatTable {
  val c1 = """
CREATE TABLE CREOLEATTRIBUTEINHERITANCE(
CREOLEATTRIBUTEINHERITANCEID NUMBER(19,0) not null,
ANCESTORATTRAVAILABILITYID NUMBER(19,0) not null,
DESCENDANTATTRAVAILABILITYID NUMBER(19,0) not null,
MIGRATIONACTION VARCHAR(10)
);

CREATE TABLE BSFDATALOADHISTORY(
LOADDATE DATE,
FILENAME VARCHAR(100),
COMMENTS VARCHAR(255),
EXCELIMAGE BLOB,
LOADFILETYPE VARCHAR(10),
LOADHISTORYID  NUMBER(19,0) not null,
USERID VARCHAR(64)
);

ALTER TABLE CREOLEATTRIBUTEINHERITANCE ADD CONSTRAINT FK_CREOLE_CAI_ACAA
FOREIGN KEY (ANCESTORATTRAVAILABILITYID) REFERENCES CREOLEATTRIBUTEAVAILABILITY(CREOLEATTRIBUTEAVAILABILITYID);

CREATE INDEX FK_CREOLE_CAI_ACAA ON CREOLEATTRIBUTEINHERITANCE(ANCESTORATTRAVAILABILITYID);

ALTER TABLE BSFDATALOADHISTORY ADD
CONSTRAINT BSFDataLoadHistory PRIMARY KEY(LOADHISTORYID );

CREATE UNIQUE INDEX REPORTNAMESTATUSIDX
ON BIREPORTCONFIGURATION(REPORTNAME, RECORDSTATUS);
  """
}

class CreatTable extends FlatSpec {

  //  "CreateTable" should "return result" in {
  //    val parser = new SQLParser
  //    val r = parser.parse(CreatTable.c1)
  //    r should not be empty
  //    r.get should have size 6
  //    forAll(r.get) { stmt ⇒
  //      stmt shouldBe a[Statement]
  //    }
  //  }
  "Test create big file" should "past all tests" in {
    val parser = new SQLParser
    val rcurrent = parser.parse(new java.io.File("./src/test/resources//CreateTablesCurrent.sql"))
    rcurrent should not be empty
    rcurrent.foreach { stmts ⇒
      val crestmt = Statement.filter[CreateStmt](stmts)
      crestmt should not be empty
      crestmt.size should be > 1000
      Statement.findTable(crestmt, "BSFSUBMITROSTERTAB") shouldBe defined
    }
    val rtarget = parser.parse(new java.io.File("./src/test/resources//CreateTablesTarget.sql"))
    rtarget should not be empty
    Comparator.findNewTables(rcurrent.get, rtarget.get) should not be empty
  }
}
