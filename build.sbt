ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.0"

lazy val root = (project in file("."))
  .settings(
    name := "scala-wordle"
  )

libraryDependencies ++= Seq(
  "com.madgag" %% "scala-collection-plus" % "0.11",
  "com.lihaoyi" %% "fansi" % "0.3.0",
  "org.roaringbitmap" % "RoaringBitmap" % "0.9.23",
  "org.scalatest" %% "scalatest" % "3.2.10" % Test
)