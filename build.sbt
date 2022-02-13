ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.1"

lazy val root = (project in file("."))
  .settings(
    name := "scala-wordle"
  )

val circeVersion = "0.14.1"

libraryDependencies ++= Seq(
  "com.madgag" %% "scala-collection-plus" % "0.11",
  "com.google.guava" % "guava" % "31.0.1-jre",
  "com.lihaoyi" %% "fansi" % "0.3.0",
  "org.scalatest" %% "scalatest" % "3.2.10" % Test
) ++ Seq("cats-core", "alleycats-core").map("org.typelevel" %% _ % "2.7.0")