// enablePlugins(ScalaJSPlugin)

name := "js2rho"
organization := "com.madmode"
version      := "0.2.0-SNAPSHOT"

// metals didn't like 2.12.6
// rchain/rchain uses 2.12.8
// https://www.scala-lang.org/ shows 2.13.1
// but de.srtobi:escalima_sjs0.6_2.13:0.5 is not available
scalaVersion := "2.12.8"

// scalaJSUseMainModuleInitializer := true

// sclajs in JVM
// fork in Test := true


// scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) }

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
// libraryDependencies += "de.srtobi" %%% "escalima" % "0.5"

// https://github.com/scala/scala-xml/wiki/Getting-started
libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.2.0"
// https://github.com/spray/spray-json
libraryDependencies += "io.spray" %%  "spray-json" % "1.3.5"
