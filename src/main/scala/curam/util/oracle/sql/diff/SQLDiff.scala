package curam.util.oracle.sql.diff

import curam.util.oracle.sql.parser.SQLParser
import curam.util.oracle.sql.parser.Comparator
import curam.util.oracle.sql.parser.CreateStmt
import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter
import curam.util.oracle.sql.parser.AlterTableStmt
import curam.util.oracle.sql.parser.CreateIndexStmt
import com.typesafe.config.ConfigFactory
import curam.util.lobs.LobReadFactory
import curam.util.lobs.LobComparator
import scala.xml.XML

/**
 * SQLDiff, compare create and alters DDL statemens and generates diff DDL
 * args: source_file, target_file, output_folder
 */
object SQLDiff extends App {
  val lobPath = "/datamanager/LobInsert.xml"
  val conf = ConfigFactory.load
  val outFolder = conf.getString("outfolder")
  val sqlSource = conf.getString("sql.source")
  val sqlTarget = conf.getString("sql.target")
  val sourceBuild = conf.getString("source.build")
  val targetBuild = conf.getString("target.build")
  val sLobPath = sourceBuild + lobPath
  val tLobPath = targetBuild + lobPath

  def emitNew(stmts: Seq[CreateStmt]) = for (st ← stmts) yield {
    s"CREATE TABLE ${st.table} (\n" + (st.props.props.map { x ⇒ x.emit }).mkString(",\n") + ");\n\n"
  }
  def emitALters(alters: Seq[Comparator.AlterTable]): Seq[String] = alters.map { x ⇒ x.emit + "\n" }
  def emitALtersTabs(alters: Seq[AlterTableStmt]): Seq[String] = alters.map { x ⇒ x.emit + "\n\n" }
  def emitCreIdx(ctxStmts: Seq[CreateIndexStmt]): Seq[String] = ctxStmts.map { ctx ⇒ ctx.emit + "\n\n" }

  val outputFolder = new File(outFolder)
  assert(outputFolder.isDirectory())
  val ddlOutName = outputFolder.getPath + "/DeltaDDL.sql"
  val writer = new BufferedWriter(new FileWriter(ddlOutName))
  val parser = new SQLParser
  // Parsers source and target
  val sourceStmts = parser.parse(sqlSource)
  val targetStmts = parser.parse(sqlTarget)
  // Diff create tables
  val newTabs = Comparator.findNewTables(sourceStmts, targetStmts)
  writer.write(emitNew(newTabs) mkString)
  // Diff modified tables
  val alterTabs = Comparator.findAlterTables(sourceStmts, targetStmts)
  writer.write(emitALters(alterTabs) mkString)
  // Diff alters constraints
  val alterDiff = Comparator.findAlterTabsDiff(sourceStmts, targetStmts)
  writer.write(emitALtersTabs(alterDiff) mkString)
  // Diff INDEXs
  val ctidxDiff = Comparator.findCreateIndexDiff(sourceStmts, targetStmts)
  writer.write(emitCreIdx(ctidxDiff) mkString)
  writer.close
  // CONTENT DATA
  val mdb = MemoryDB(sourceStmts.get)
  //  val exludeTabs = Seq("APPRESOURCE", "KEYSERVER", "PRODUCTPROVIDER")
  val exludeTabs = Seq("KEYSERVER", "PRODUCTPROVIDER")
  val exludedDiffFields = Seq("LASTWRITTEN")
  val deltaOutName = outputFolder.getPath + "/deltaData.sql"
  val deltaWriter = new BufferedWriter(new FileWriter(deltaOutName))
  val diff = mdb.diffAndWrite(targetStmts, Option(MemoryDB.ExcludeOption(exludeTabs, exludedDiffFields)), deltaWriter)
  deltaWriter.close
  // LOB DATA
  val sourceLobs = LobReadFactory(sLobPath)
  val targetLobs = LobReadFactory(tLobPath)
  val outLobs = LobComparator.compare(sourceLobs, targetLobs, mdb.pks)
  val outLobXml = <root><lob>{ outLobs.map { _.node } }</lob></root>
  XML.save(s"${outFolder}/LobInsert.xml", outLobXml, "UTF-8", true, null)
}
