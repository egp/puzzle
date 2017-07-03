import sbt._

object Dependencies {

  val scalatestVersion = "3.0.3"
  object External {
    lazy val scalactic = "org.scalactic" %% "scalactic" % scalatestVersion
  }

  object ExternalTest {
    lazy val scalaTest = "org.scalatest" %% "scalatest" % scalatestVersion
  }

}