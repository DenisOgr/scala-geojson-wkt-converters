ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
      name := "scala-geo-json",
    libraryDependencies ++= Seq(
      "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.15.1",
      "org.scalatest" %% "scalatest-flatspec" % "3.2.17" % "test"
    )
  )
