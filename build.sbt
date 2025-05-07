Global / onChangedBuildSource := ReloadOnSourceChanges

val scalaV = "3.3.5"

ThisBuild / scalaVersion := scalaV
ThisBuild / crossScalaVersions := Seq(scalaV)

// GitHub Actions config
val javaVersions = Seq(8, 11, 17, 21).map(v => JavaSpec.temurin(v.toString))

ThisBuild / githubWorkflowJavaVersions := javaVersions
ThisBuild / githubWorkflowArtifactUpload := false
ThisBuild / githubWorkflowBuildMatrixFailFast := Some(false)
ThisBuild / githubWorkflowTargetBranches := Seq("master")

ThisBuild / githubWorkflowBuild := Seq(
  WorkflowStep.Run(List("cd tests && yarn && cd .."), name = Some("yarn install")),
  WorkflowStep.Sbt(List("test"), name = Some("test")),
)

ThisBuild / githubWorkflowPublishTargetBranches := Seq()

lazy val cats = "org.typelevel" %% "cats-core" % "2.13.0"
def circe(proj: String) = "io.circe" %% s"circe-$proj" % "0.14.10"
lazy val joda = "joda-time" % "joda-time" % "2.13.0"
def munit(proj: String = "") = "org.scalameta" %% s"munit${if (proj == "") "" else s"-$proj"}" % "1.1.0" % Test
lazy val scalacheck = "org.scalacheck" %% "scalacheck" % "1.18.1" % Test
lazy val scalaz = "org.scalaz" %% "scalaz-core" % "7.3.8"
lazy val slf4j = "org.slf4j" % "slf4j-api" % "2.0.16"

lazy val root = project.in(file("."))
  .settings(
    name := "scala-ts",
    organization := "bondlink",
    version := "0.19.0",
    scalaVersion := scalaV,

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
    publishTo := Some("BondLink S3".at("s3://bondlink-maven-repo")),
    licenses += ("MIT", url("https://opensource.org/licenses/MIT")),
  )
  .enablePlugins(BuildInfoPlugin)
