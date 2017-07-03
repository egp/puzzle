import Dependencies._

lazy val puzzle = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "net.egp",
      scalaVersion := "2.12.2",
      version      := "0.2.0-SNAPSHOT"
    )),
    name := "Puzzle",
    libraryDependencies ++= Seq(
      ExternalTest.scalaTest % Test,
      External.scalactic
    )
  )
