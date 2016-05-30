package curam.util.oracle.sql.diff

import java.io.File
import curam.util.oracle.sql.parser.SQLParser
import curam.util.oracle.sql.parser.PrimaryKeyDef
import curam.util.oracle.sql.parser.Comparator
import curam.util.oracle.sql.parser.InsertIntoStmt
import java.io.BufferedWriter
import jdk.internal.jfr.events.FileWriteEvent
import java.io.FileWriter

/**
 * Compare codetable build inserts
 * args: source_folder, target_foulder, file_output
 *
 */
object CodeTableDiff extends App {

  val pks = Seq(PrimaryKeyDef("CODETABLEDATA", Seq("TABLENAME", "LOCALEIDENTIFIER")),
    PrimaryKeyDef("CODETABLEHEADER", Seq("TABLENAME")),
    PrimaryKeyDef("CODETABLEHIERARCHY", Seq("HIERARCHYNAME")),
    PrimaryKeyDef("CODETABLEITEM", Seq("TABLENAME", "CODE", "LOCALEIDENTIFIER")),
    PrimaryKeyDef("CTDISPLAYNAME", Seq("TABLENAME", "LOCALEIDENTIFIER")))

  def checkFolder(folder: String): List[File] = {
    val fold = new File(folder)
    require(fold.isDirectory())
    fold.listFiles.filter { f ⇒ f.isFile && f.getName.endsWith(".sql") }.toList
  }
  def parserFiles(files: List[File]) = {
    (for { file ← files } yield {
      val parser = new SQLParser
      parser.parse(file).getOrElse(Seq())
    }).flatten
  }
  require(args.length == 3)
  val sourceFiles = checkFolder(args(0))
  val targetFiles = checkFolder(args(1))
  val sourceStmts: Seq[InsertIntoStmt] = parserFiles(sourceFiles)
  val targetStmts: Seq[InsertIntoStmt] = parserFiles(targetFiles)
  val memoryDB = new MemoryDB(pks, sourceStmts)
  val diff: Seq[Comparator.WriteDelta] = for {
    target ← targetStmts
    found = memoryDB.find(target)
    if (found == None || !target.allValuesAreEqual(found.get))
  } yield {
    found match {
      case None    ⇒ Comparator.WriteInsert(target)
      case Some(x) ⇒ Comparator.WrtieUpdate(target)
    }
  }
  // val writer = new BufferedWriter(new FileWriter(args(2)))
  println(s"Found ${diff.size} differences")
}
