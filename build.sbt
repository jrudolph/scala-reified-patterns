libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
  "org.specs2" %% "specs2-core" % "3.8.9" % "test"
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

scalaVersion := "2.12.2"

scalacOptions ++= Seq("-deprecation", "-feature", "-encoding", "utf8")