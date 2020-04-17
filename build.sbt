import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.madmode",
      // metals didn't like 2.12.6
      // rchain/rchain uses 2.12.8
      scalaVersion := "2.12.8",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "tinyses2rho",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "de.srtobi" %% "escalima" % "0.3",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"
  )
