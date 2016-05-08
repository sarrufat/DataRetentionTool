package com.sirec.sql.parser

object Ast {
  case class identifier(name: String)
  /*
   * Statement
   */
  sealed trait stmt

  object stmt {
    case class CreateTable(table: identifier)
    case class relationalProperties(properties: Seq[columnDefinition])
    case class columnDefinition(column: identifier, dataType: String)
  }
}
