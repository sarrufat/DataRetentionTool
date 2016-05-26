package curam.util.oracle.sql

package object parser {
  implicit def toCreateStmt(stmts: Option[Seq[Statement]]): Seq[CreateStmt] = Statement.filter[CreateStmt](stmts.getOrElse(Seq()))
  implicit def toAlterStmt(stmts: Option[Seq[Statement]]): Seq[AlterTableStmt] = Statement.filter[AlterTableStmt](stmts.getOrElse(Seq()))
  implicit def toCreIDXStmt(stmts: Option[Seq[Statement]]): Seq[CreateIndexStmt] = Statement.filter[CreateIndexStmt](stmts.getOrElse(Seq()))
}
