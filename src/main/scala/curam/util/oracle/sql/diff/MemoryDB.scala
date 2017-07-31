package curam.util.oracle.sql.diff

import curam.util.oracle.sql.parser.PrimaryKeyDef
import curam.util.oracle.sql.parser.InsertIntoStmt
import curam.util.oracle.sql.parser.PrimaryKeyDef
import curam.util.oracle.sql.parser.PrimaryKeyDef
import curam.util.oracle.sql.parser.PrimaryKeyDef
import curam.util.oracle.sql.parser.InsertIntoStmt
import curam.util.oracle.sql.parser.InsertIntoStmt
import curam.util.oracle.sql.parser.Comparator
import curam.util.oracle.sql.parser.PrimaryKey
import curam.util.oracle.sql.parser.AlterTableStmt
import curam.util.oracle.sql.parser.PrimaryKey
import curam.util.oracle.sql.parser.AlterTableStmt
import curam.util.oracle.sql.parser.PrimaryKeyDef
import curam.util.oracle.sql.parser.Statement
import java.io.BufferedWriter
import curam.util.oracle.sql.parser.ForeignKey
import curam.util.oracle.sql.parser.AlterTableStmt
import curam.util.oracle.sql.parser.ForeignKeyDef
import curam.util.oracle.sql.parser.Comparator.WriteInsert
import curam.util.oracle.sql.parser.Comparator.WrtieUpdate
import scala.annotation.tailrec

class MemoryDB(val pks: Seq[PrimaryKeyDef], val statements: Seq[InsertIntoStmt], fks: Seq[ForeignKeyDef]) {
  private class TableIndex(val pk: PrimaryKeyDef, val statements: Seq[InsertIntoStmt]) {

    private def getKeyValues(insert: InsertIntoStmt): Seq[String] = {
      pkMap.get(insert.table) match {
        case Some(pk) ⇒ pk.columns.map { insert.getValue(_) }
        case None     ⇒ Seq()
      }
    }

    val pkTableMap = statements.map { insert ⇒ (getKeyValues(insert).mkString, insert) } toMap
    def find(insert: InsertIntoStmt): Option[InsertIntoStmt] = {
      pkTableMap get (getKeyValues(insert) mkString) match {
        case Some(ins) ⇒ Option(ins)
        case None      ⇒ None
      }
    }
  }

  // Table,PrimaryKeyDef map
  private val pkMap: Map[String, PrimaryKeyDef] = pks.map { x ⇒ (x.table, x) }.toMap
  // Get key values as a list
  // Group sentences by tables
  private val tableGoups = statements.groupBy { ins ⇒ ins.table }
  private val indexMap = tableGoups.map { tg ⇒ (tg._1, new TableIndex(pkMap.getOrElse(tg._1, PrimaryKeyDef(tg._1, Seq())), tg._2)) } toMap

  // Foreign key map
  private val fkMap: Map[String, ForeignKeyDef] = fks.map { fk ⇒ (fk.tab, fk) }.toMap
  private def getPropsValues(insert: InsertIntoStmt, propnames: Seq[String]) = propnames.map { insert.getValue(_) }

  def find(insert: InsertIntoStmt): Option[InsertIntoStmt] = indexMap.get(insert.table).flatMap { tabidx ⇒ tabidx.find(insert) }

  private def dependecySortWriters(writers: Seq[Comparator.WriteDelta]): Seq[Comparator.WriteDelta] = {
      @tailrec
      def internalSort(emited: List[Comparator.WriteDelta], pending: List[Comparator.WriteDelta]): List[Comparator.WriteDelta] = {
        pending match {
          case h :: Nil ⇒ emited :+ h
          case h :: xs ⇒ {
            fkMap.get(h.getInsert.table) match {
              case Some(fk) ⇒ {
                val refTab = fk.fk.reference.obj
                xs.filter { x ⇒ x.getInsert.table == refTab }.find { inst ⇒ getPropsValues(inst.getInsert, fk.fk.reference.columns) == getPropsValues(h.getInsert, fk.fk.columns) } match {
                  case Some(inst) ⇒ internalSort(emited ++ List(inst, h), xs.filterNot { x ⇒ x == inst })
                  case None       ⇒ internalSort(emited :+ h, xs)
                }
              }
              case None ⇒ internalSort(emited :+ h, xs)
            }
          }
          case Nil ⇒ emited
        }
      }
    internalSort(List(), writers.toList)
  }
  // Diff insert statements with foreign key dependencies
  def diff(targetStmts: Seq[InsertIntoStmt], excl: Option[MemoryDB.ExcludeOption] = None): Seq[Comparator.WriteDelta] = {
    val result = for {
      target ← targetStmts
      options ← excl
      if (excl == None || !options.tabs.contains(target.table))
      found = find(target)
      if (found == None || (excl == None && !target.allValuesAreEqual(found.get)) || !target.allValuesAreEqual(found.get, excl.get.fields))
      if (!(found != None && options.insertOnlyTabs.contains(target.table)))
    } yield {
      found match {
        case None    ⇒ Comparator.WriteInsert(target)
        case Some(x) ⇒ Comparator.WrtieUpdate(target, pkMap.get(x.table));
      }
    }
    dependecySortWriters(result)
  }
  def diffAndWrite(targetStmts: Seq[InsertIntoStmt], excl: Option[MemoryDB.ExcludeOption], bwr: BufferedWriter): Seq[Comparator.WriteDelta] = {
    val d = diff(targetStmts, excl)
    d.foreach { wr ⇒ bwr.write(wr.emit + "\n") }
    val appendStmts = """
INSERT INTO securityfidsid
  (sidname,fidname
  )
SELECT f.fidname,
  f.fidname
FROM functionidentifier f
WHERE NOT EXISTS
  (SELECT * FROM securityfidsid WHERE sidname = f.fidname
  );
INSERT INTO securityidentifier
  (sidname,sidtype, versionNo
  )
SELECT sidname,
  'FUNCTION',
  0
FROM securityfidsid s1
WHERE NOT EXISTS
  ( SELECT * FROM securityidentifier s2 WHERE s2.sidname = s1.sidname
  );
INSERT INTO securitygroupsid
  (groupname, sidname
  )
SELECT 'SUPERGROUP',
  sidname
FROM securityidentifier si
WHERE NOT EXISTS
  (SELECT * FROM securitygroupsid si2 WHERE si2.sidname = si.sidname
  );
INSERT
INTO BATCHPROCDESC
  (
    PROCESSDEFNAME,
    PROCESSLONGNAME,
    DESCRIPTION,
    BATCHTYPE,
    VERSIONNO
  )
SELECT PROCESSDEFNAME,
  PROCESSDEFNAME,
  RTRIM(PROCESSDEFNAME),
  '',
  1
FROM BATCHPROCDEF B1
WHERE NOT EXISTS
  (SELECT * FROM BATCHPROCDESC B2 WHERE B1.PROCESSDEFNAME = B2.PROCESSDEFNAME
  );
INSERT INTO BATCHPARAMDESC
  (PARAMNAME, PROCESSDEFNAME, DESCRIPTION, VERSIONNO
  )
SELECT PARAMNAME,
  PROCESSDEFNAME,
  RTRIM(PARAMTYPE),
  1
FROM BATCHPARAMDEF B1
WHERE NOT EXISTS
  (SELECT * FROM BATCHPARAMDESC B2 WHERE B2.PROCESSDEFNAME = B1.PROCESSDEFNAME
  );
UPDATE BATCHPARAMDESC SET DEFAULTVALUE = NULL;
      """
    bwr.write(appendStmts)
    bwr.close
    d
  }
}

object MemoryDB {
  case class ExcludeOption(val tabs: Seq[String], val fields: Seq[String], val insertOnlyTabs: Seq[String])

  def apply(statements: Seq[Statement], targets: Seq[Statement]) = {
    val pks = statements.collect { case AlterTableStmt(tab, pk: PrimaryKey) ⇒ PrimaryKeyDef(tab, pk.columns) }
    val foreignks = targets.collect { case AlterTableStmt(tab, fk: ForeignKey) ⇒ ForeignKeyDef(tab, fk) }
    new MemoryDB(pks, statements, foreignks)
  }
}
