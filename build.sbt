name := "CodeSheet"
  
version := "0.5"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-compiler" % "2.10.3",
  "com.github.jedesah" %% "codesheet-api" % "0.5"
)

play.Project.playScalaSettings

scalaVersion := "2.10.3"

seq(bintrayResolverSettings:_*)

resolvers += bintray.Opts.resolver.repo("jedesah", "maven")