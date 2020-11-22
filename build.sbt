ThisBuild / organization := "io.github.agobi"
ThisBuild / homepage := Some(url("https://github.com/agobi/tlm"))
ThisBuild / licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))
ThisBuild / developers := List(Developer("agobi", "Attila Góbi", "attila.gobi@gmail.com", url("https://github.com/agobi")))
ThisBuild / scmInfo := Some(ScmInfo(url("https://github.com/agobi/tlm"), "scm:git:git@github.com:agobi/tlm.git"))

ThisBuild / scalaVersion := "2.13.4"



enablePlugins(ScalaJSBundlerPlugin)
name := "tlm"
scalaJSUseMainModuleInitializer := true
requireJsDomEnv in Test := true
useYarn := true

scalacOptions --= {
  if (insideCI.value) Nil
  else List("-Xfatal-warnings")
}

libraryDependencies ++= List(
  "org.scala-js" %%% "scalajs-dom" % "1.1.0",
  "com.lihaoyi" %%% "utest" % "0.7.4" % "test"
)

testFrameworks += new TestFramework("utest.runner.Framework")

