scalaVersion := "3.1.2"

// Set to false or remove if you want to show stubs as linking errors
nativeLinkStubs := true
libraryDependencies += "org.scalaz" %%% "scalaz-core" % "7.3.6"
libraryDependencies += "org.scalaz" %%% "scalaz-effect" % "7.3.6"

scalacOptions += "-feature"
console / initialCommands  := "import scalaz._, Scalaz._"

addCompilerPlugin("org.scala-native" % "junit-plugin" % "0.4.3" cross CrossVersion.full)
libraryDependencies += "org.scala-native" %%% "junit-runtime" % "0.4.3" % Test
testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-s", "-v")

enablePlugins(ScalaNativePlugin)
