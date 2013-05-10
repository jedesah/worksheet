import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "CodeSheet"
  val appVersion      = "0.2-SNAPSHOT"

  val appDependencies = Seq(
    "org.scala-lang" % "scala-compiler" % "2.10.0",
    jdbc,
    anorm
  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    testOptions in Test += Tests.Argument("junitxml", "console")
  )

}
