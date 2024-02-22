Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val scalaVersions = Seq("2.12.18", "2.13.11", "3.3.1")

def foldScalaV[A](scalaVersion: String)(on2: => A, on3: => A): A =
  scalaVersion match {
    case s if s.startsWith("2.") => on2
    case s if s.startsWith("3.") => on3
  }

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
    crossScalaVersions := scalaVersions,
    scalaVersion := scalaVersions.find(_.startsWith("3.")).get,

    Test / scalacOptions += "-Yretain-trees",
    Compile / doc / scalacOptions ++= foldScalaV(scalaVersion.value)(
      Seq(),
      Seq(
        "-skip-by-regex:^scalats\\.BuildInfo\\$$",
      ),
    ),

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
    ) ++ foldScalaV(scalaVersion.value)(
      Seq(
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        "ch.qos.logback" % "logback-classic" % "1.2.3",
      ),
      Seq(),
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
