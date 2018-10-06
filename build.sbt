

lazy val root = (project in file(".")).
  settings(
    name := "DataRetentionTool",
    version := "1.1.RGC",
    scalaVersion := "2.11.8"
  )

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/releases"

// libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.3.5"

// libraryDependencies += "us.fatehi" % "schemacrawler" % "12.06.03"

// libraryDependencies += "us.fatehi" % "schemacrawler-oracle" % "12.06.03"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.2"


libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.6" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"

libraryDependencies += "com.typesafe" % "config" % "1.2.1"


libraryDependencies += "com.storm-enroute" %% "scalameter-core" % "0.8.2"

EclipseKeys.withBundledScalaContainers := false

retrieveManaged := true
