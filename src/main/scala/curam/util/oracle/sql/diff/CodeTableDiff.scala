package curam.util.oracle.sql.diff

import java.io.File
import curam.util.oracle.sql.parser.SQLParser

/**
 * Compare codetable build inserts
 * args: source_folder, target_foulder, file_output
 *
 */
class CodeTableDiff extends App {

  def checkFolder(folder: String): List[File] = {
    val fold = new File(folder)
    require(fold.isDirectory())
    fold.listFiles.filter { f ⇒ f.isFile && f.getName.endsWith(".sql") }.toList
  }
  def parserFiles(files: List[File]) = {
    for { file ← files } yield {
      val parser = new SQLParser
      parser.parse(file)
    }
  }
  require(args.length == 3)
  val sourceFiles = checkFolder(args(0))
  val targetFiles = checkFolder(args(1))
  val sourceStmts = parserFiles(sourceFiles)
  val targetStmts = parserFiles(targetFiles)
}
