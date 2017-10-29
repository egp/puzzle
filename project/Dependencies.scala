import sbt._

object Dependencies {

  val scalatestVersion = "3.0.4"
  val betterFilesVersion = "3.2.0"

  object External {
    lazy val scalactic: ModuleID = "org.scalactic" %% "scalactic" % scalatestVersion
    lazy val betterFiles: ModuleID = "com.github.pathikrit" %% "better-files" % betterFilesVersion
  }

  object ExternalTest {
    lazy val scalaTest: ModuleID = "org.scalatest" %% "scalatest" % scalatestVersion
  }

}