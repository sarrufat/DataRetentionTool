package curam.util.oracle.sql.parser

trait PrettyPrinters {
  // TODO: escape
  protected def _q(s: String): String = "\"" + s + "\""
}
