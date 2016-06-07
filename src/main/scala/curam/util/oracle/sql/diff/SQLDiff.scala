package curam.util.oracle.sql.diff

import curam.util.oracle.sql.parser.SQLParser
import curam.util.oracle.sql.parser.Comparator
import curam.util.oracle.sql.parser.CreateStmt
import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter
import curam.util.oracle.sql.parser.AlterTableStmt
import curam.util.oracle.sql.parser.CreateIndexStmt

/**
 * SQLDiff, compare create and alters DDL statemens and generates diff DDL
 * args: source_file, target_file, output_folder
 */
object SQLDiff extends App {
  def emitNew(stmts: Seq[CreateStmt]) = for (st ← stmts) yield {
    s"CREATE TABLE ${st.table} (\n" + (st.props.props.map { x ⇒ x.emit }).mkString(",\n") + ");\n\n"
  }
  def emitALters(alters: Seq[Comparator.AlterTable]): Seq[String] = alters.map { x ⇒ x.emit + "\n" }
  def emitALtersTabs(alters: Seq[AlterTableStmt]): Seq[String] = alters.map { x ⇒ x.emit + "\n\n" }
  def emitCreIdx(ctxStmts: Seq[CreateIndexStmt]): Seq[String] = ctxStmts.map { ctx ⇒ ctx.emit + "\n\n" }
  assert(args.length == 3)
  val outputFolder = new File(args(2))
  assert(outputFolder.isDirectory())
  val ddlOutName = outputFolder.getPath + "/DeltaDDL.sql"
  val writer = new BufferedWriter(new FileWriter(ddlOutName))
  val parser = new SQLParser
  val sourceStmts = parser.parse(args(0))
  val targetStmts = parser.parse(args(1))
  val newTabs = Comparator.findNewTables(sourceStmts, targetStmts)
  writer.write(emitNew(newTabs) mkString)
  val alterTabs = Comparator.findAlterTables(sourceStmts, targetStmts)
  writer.write(emitALters(alterTabs) mkString)
  val alterDiff = Comparator.findAlterTabsDiff(sourceStmts, targetStmts)
  writer.write(emitALtersTabs(alterDiff) mkString)
  val ctidxDiff = Comparator.findCreateIndexDiff(sourceStmts, targetStmts)
  writer.write(emitCreIdx(ctidxDiff) mkString)
  writer.close
  val mdb = MemoryDB(sourceStmts.get)
  //  val exludeTabs = Seq("APPRESOURCE", "KEYSERVER", "PRODUCTPROVIDER")
  val exludeTabs = Seq("KEYSERVER", "PRODUCTPROVIDER")
  val exludedDiffFields = Seq("LASTWRITTEN")
  val deltaOutName = outputFolder.getPath + "/deltaData.sql"
  val deltaWriter = new BufferedWriter(new FileWriter(deltaOutName))
  val diff = mdb.diffAndWrite(targetStmts, Option(MemoryDB.ExcludeOption(exludeTabs, exludedDiffFields)), deltaWriter)
  deltaWriter.close
}
