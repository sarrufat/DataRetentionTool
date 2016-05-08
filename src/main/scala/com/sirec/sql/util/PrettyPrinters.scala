package com.sirec.sql.util

trait PrettyPrinters {
  // TODO: escape
  protected def _q(s: String): String = "\"" + s + "\""
}
