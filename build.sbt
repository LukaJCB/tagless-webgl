enablePlugins(ScalaJSPlugin)

name := "tagless-webgl"

version := "1.0"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "org.typelevel" %%% "cats-core" % "1.0.0-RC1",
  "org.typelevel" %%% "cats-effect" % "0.5",
  "org.scala-js" %%% "scalajs-dom" % "0.9.2",
  "co.fs2" %%% "fs2-core" % "0.10.0-M8"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

scalaJSUseMainModuleInitializer := true

jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()

