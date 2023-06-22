import sbt.Keys._

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val scalaVersions = Seq("2.12.18", "2.13.11", "3.3.0")

def foldScalaV[A](scalaVersion: String)(on2: => A, on3: => A): A =
  scalaVersion match {
    case s if s.startsWith("2.") => on2
    case s if s.startsWith("3.") => on3
  }

lazy val root = (project in file(".")).
  settings(Seq(
    name := "scala-ts",
    organization := "bondlink",
    crossScalaVersions := scalaVersions,
    scalaVersion := scalaVersions.find(_.startsWith("3.")).get,
    scalacOptions ++= foldScalaV(scalaVersion.value)(
      Seq(),
      Seq(
        "-Wvalue-discard",
        "-Wunused:implicits",
        "-Wunused:imports",
        "-Wunused:locals",
        "-Wunused:params",
        "-Wunused:privates",
        "-Wunused:unsafe-warn-patvars",
      ),
    ),
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % "0.14.3",
      "joda-time" % "joda-time" % "2.12.5",
      "org.typelevel" %% "cats-core" % "2.9.0",
      "org.scalaz" %% "scalaz-core" % "7.3.6",
    ) ++ foldScalaV(scalaVersion.value)(
      Seq(
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        "ch.qos.logback" % "logback-classic" % "1.2.3",
      ),
      Seq(),
    ),

    // Publish settings
    publish / skip := false,
    publishMavenStyle := true,
    Test / publishArtifact := false,
    gitPublishDir := file("/src/maven-repo"),
    licenses += ("MIT", url("https://opensource.org/licenses/MIT")),
  ))
