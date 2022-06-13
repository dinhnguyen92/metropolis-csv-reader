ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

val tototoshi = "com.github.tototoshi"
val tototoshiVersion = "1.3.10"

val scalactic = "org.scalactic"
val scalacticVersion = "3.2.12"

val scalaTest = "org.scalatest"
val scalaTestVersion = "3.2.12"

val jodaTime = "joda-time"
val jodaTimeVersion = "2.10.14"

libraryDependencies ++= Seq(
  tototoshi %% "scala-csv" % tototoshiVersion,
  scalactic %% "scalactic" % scalacticVersion,
  scalaTest %% "scalatest" % scalaTestVersion % "test",
  jodaTime % "joda-time" % jodaTimeVersion
)

lazy val root = (project in file("."))
  .settings(
    name := "Metropolis"
  )
