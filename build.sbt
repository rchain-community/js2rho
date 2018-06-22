import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.madmode",
      scalaVersion := "2.12.6",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "tinyses2rho",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"
  )
