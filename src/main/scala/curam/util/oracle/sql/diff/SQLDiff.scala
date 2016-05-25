package curam.util.oracle.sql.diff

import curam.util.oracle.sql.parser.SQLParser
import curam.util.oracle.sql.parser.Comparator
import curam.util.oracle.sql.parser.CreateStmt
import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter

object SQLDiff extends App {
  def emitNew(stmts: Seq[CreateStmt]) = for (st ← stmts) yield {
    s"CREATE TABLE ${st.table} (\n" + (st.props.props.map { x ⇒ x.emit }).mkString(",\n") + ");\n"
  }
  def emitALters(alters: Seq[Comparator.AlterTable]): Seq[String] = alters.map { x ⇒ x.emit }
  assert(args.length == 3)

  val writer = new BufferedWriter(new FileWriter(args(2)))
  val parser = new SQLParser
  val sourceStmts = parser.parse(args(0))
  val targetStmts = parser.parse(args(1))
  val newTabs = Comparator.findNewTables(sourceStmts.get, targetStmts.get)
  writer.write(emitNew(newTabs) mkString)
  val alterTabs = Comparator.findAlterTables(sourceStmts.get, targetStmts.get)
  writer.write(emitALters(alterTabs) mkString)
  writer.close
}
