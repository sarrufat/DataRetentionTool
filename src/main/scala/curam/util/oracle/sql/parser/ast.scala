package curam.util.oracle.sql.parser

trait Node extends PrettyPrinters {

  def sql: String
}

case class CreateStmt(tables: String, body: String)

trait CreateTableExpr extends Node {
  def getTable: String
  def getBody: String
}
