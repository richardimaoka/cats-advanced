import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.3",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "cats-advanced",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "1.0.0-MF",
      scalaTest % Test,
      "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
    )
  )
