package curam.util.oracle.cleaner

import scala.io.Source
import java.io.PrintWriter
import java.io.File

// Skip INSERT LINES UNTIL ');'
object LineCleaner extends App {

  def cleanFile(source: String, output: String) = {
    val ss = Source.fromFile(source)
    val pw = new PrintWriter(new File(output))
    var skipNext = false
    for (line ← ss.getLines()) {
      if (skipNext) {
        skipNext = false
      } else if (!(line.startsWith("INSERT INTO") || line.startsWith("insert into"))) {
        pw.println(line)
      } else if (!line.endsWith(");")) {
        skipNext = true
      }
    }
    pw.close()
  }
  assert(args.size == 2)
  cleanFile(args(0), args(1))
  // cleanFile("./src/test/resources/SetupDatabaseCleanTest.sql")
}
