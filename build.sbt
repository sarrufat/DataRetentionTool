

lazy val root = (project in file(".")).
  settings(
    name := "DataRetentionTool",
    version := "1.0",
    scalaVersion := "2.11.7"
  )


// libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.3.5"

// libraryDependencies += "us.fatehi" % "schemacrawler" % "12.06.03"

// libraryDependencies += "us.fatehi" % "schemacrawler-oracle" % "12.06.03"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.6" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"



