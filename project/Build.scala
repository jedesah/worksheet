import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "repl"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    "org.scala-lang" % "scala-compiler" % "2.10.0",
    jdbc,
    anorm
  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    // Add your own project settings here      
  )

}
