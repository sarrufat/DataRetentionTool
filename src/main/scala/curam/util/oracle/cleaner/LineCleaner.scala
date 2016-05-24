package curam.util.oracle.cleaner

import scala.io.Source

// Skip INSERT LINES UNTIL ');'
object LineCleaner extends App {

  def cleanFile(source: String) = {
    val ss = Source.fromFile(source)
    for (line ← ss.getLines()) {
      if (!(line.startsWith("INSERT INTO") || line.startsWith("insert into"))) {
        println(line);
      }
    }
  }
  assert(args.size == 1)
  cleanFile(args(0))
  // cleanFile("./src/test/resources/SetupDatabaseCleanTest.sql")
}
