package curam.util.oracle.sql.diff

import curam.util.oracle.sql.parser.SQLParser
import curam.util.oracle.sql.parser.Comparator

object SQLDiff extends App {
  assert(args.length == 3)
  val parser = new SQLParser
  val sourceStmts = parser.parse(new java.io.File(args(0)))
  val targetStmts = parser.parse(new java.io.File(args(1)))
  Comparator.findNewTables(sourceStmts.get, targetStmts.get)
}
