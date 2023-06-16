import sbt.Keys._

Global / onChangedBuildSource := ReloadOnSourceChanges

val publishSettings = Seq(
  publish / skip := false,
  publishMavenStyle := true,
  Test / publishArtifact := false,
  gitPublishDir := file("/src/maven-repo"),
  licenses += ("MIT", url("https://opensource.org/licenses/MIT"))
)

lazy val root = project.in(file("."))
  .settings(
    name := "scala-ts",
    organization := "bondlink",
    version := "0.9.5-BL",
    scalaVersion := "3.3.0",
    scalacOptions ++= Seq(
      "-Wvalue-discard",
      "-Wunused:implicits",
      "-Wunused:imports",
      "-Wunused:locals",
      "-Wunused:params",
      "-Wunused:privates",
      "-Wunused:unsafe-warn-patvars",
    ),

    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % "0.14.3",
      "joda-time" % "joda-time" % "2.12.5",
      "org.typelevel" %% "cats-core" % "2.9.0",
      // Scalaz is just needed for checking whether values are types like `\/` and `\&/`
      "org.scalaz" %% "scalaz-core" % "7.3.6",
    ),

    // Publish settings
    publish / skip := false,
    publishMavenStyle := true,
    Test / publishArtifact := false,
    gitPublishDir := file("/src/maven-repo"),
    licenses += ("MIT", url("https://opensource.org/licenses/MIT"))
  )
