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

class MemoryDB(val pks: Seq[PrimaryKeyDef], val statements: Seq[InsertIntoStmt]) {
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

  def find(insert: InsertIntoStmt): Option[InsertIntoStmt] = indexMap.get(insert.table).flatMap { tabidx ⇒ tabidx.find(insert) }

  def diff(targetStmts: Seq[InsertIntoStmt]): Seq[Comparator.WriteDelta] = for {
    target ← targetStmts
    found = find(target)
    if (found == None || !target.allValuesAreEqual(found.get))
  } yield {
    found match {
      case None    ⇒ Comparator.WriteInsert(target)
      case Some(x) ⇒ Comparator.WrtieUpdate(target, pkMap.get(x.table));
    }
  }
}
