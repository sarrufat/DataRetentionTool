package curam.util.oracle.sql

package object parser {
  implicit def toCreateStmt(stmts: Seq[Statement]): Seq[CreateStmt] = Statement.filter[CreateStmt](stmts)
}
