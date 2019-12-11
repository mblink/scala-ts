import sbt.Keys._

val publishSettings = Seq(
  skip in publish := false,
  bintrayOrganization := Some("bondlink"),
  bintrayRepository := "scala-ts",
  bintrayReleaseOnPublish in ThisBuild := false,
  licenses += ("MIT", url("https://opensource.org/licenses/MIT"))
)

lazy val root = (project in file(".")).
  settings(Seq(
    name := "scala-ts",
    organization := "com.github.miloszpp",
    mainClass in (Compile, run) := Some("com.mpc.scalats.Main"),
    sbtPlugin := true,
    scalaVersion := "2.12.10",
    crossScalaVersions := Seq("2.10.7", scalaVersion.value),
    sbtVersion in pluginCrossBuild := {
      scalaBinaryVersion.value match {
        case "2.10" => "0.13.16"
        case "2.12" => "1.3.4"
      }
    }) ++ publishSettings)

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "org.scalatest" %% "scalatest" % "3.1.0" % "test"
)