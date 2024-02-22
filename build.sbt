Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val cats = "org.typelevel" %% "cats-core" % "2.10.0"
def circe(proj: String) = "io.circe" %% s"circe-$proj" % "0.14.6"
lazy val joda = "joda-time" % "joda-time" % "2.12.7"
def munit(proj: String = "") = "org.scalameta" %% s"munit${if (proj == "") "" else s"-$proj"}" % "1.0.0-M11" % Test
lazy val scalacheck = "org.scalacheck" %% "scalacheck" % "1.17.0" % Test
lazy val scalaz = "org.scalaz" %% "scalaz-core" % "7.3.8"
lazy val slf4j = "org.slf4j" % "slf4j-api" % "2.0.12"

lazy val root = project.in(file("."))
  .settings(
    name := "scala-ts",
    organization := "bondlink",
    version := "0.13.0",
    scalaVersion := "3.3.1",

    Test / scalacOptions += "-Yretain-trees",
    Compile / doc / scalacOptions += "-skip-by-regex:^scalats\\.BuildInfo\\$$",

    libraryDependencies ++= Seq(
      cats,
      // Optional dependencies to provide more scala => TS type support
      circe("core") % Optional,
      joda % Optional,
      scalaz % Optional,
      // Test dependencies
      circe("parser") % Test,
      munit(),
      munit("scalacheck"),
      scalacheck,
      slf4j,
    ),

    buildInfoPackage := "scalats",
    buildInfoKeys := Seq(BuildInfoKey.action("testsDir")(baseDirectory.value / "tests")),

    // Publish settings
    publishMavenStyle := true,
    Test / publishArtifact := false,
    gitPublishDir := file("/src/maven-repo"),
    licenses += ("MIT", url("https://opensource.org/licenses/MIT")),
  )
  .enablePlugins(BuildInfoPlugin)
