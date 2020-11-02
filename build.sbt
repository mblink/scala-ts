import sbt.Keys._

Global / onChangedBuildSource := ReloadOnSourceChanges

val publishSettings = Seq(
  skip in publish := false,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  bintrayOrganization := Some("bondlink"),
  bintrayRepository := "scala-ts",
  bintrayReleaseOnPublish in ThisBuild := false,
  licenses += ("MIT", url("https://opensource.org/licenses/MIT"))
)

lazy val pomSettings = Seq(
  pomExtra :=
    <url>https://github.com/mblink/scala-ts</url>
    <licenses>
      <license>
        <name>MIT</name>
        <url>https://opensource.org/licenses/MIT</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:mblink/scala-ts.git</url>
      <connection>scm:git:git@github.com:mblink/scala-ts.git</connection>
    </scm>
    <developers>
      <developer>
        <id>miloszpp</id>
        <name>Miłosz Piechocki</name>
        <url>http://codewithstyle.info</url>
      </developer>
      <developer>
        <id>jleider</id>
        <name>Justin Leider</name>
      </developer>
    </developers>
)

lazy val scalaVersions = Seq("2.12.11", "2.13.2")

lazy val root = (project in file(".")).
  settings(Seq(
    name := "scala-ts",
    organization := "com.github.miloszpp",
    mainClass in (Compile, run) := Some("com.mpc.scalats.Main"),
    crossScalaVersions := scalaVersions,
    scalaVersion := scalaVersions.find(_.startsWith("2.13")).get,
    sbtVersion in pluginCrossBuild := {
      scalaBinaryVersion.value match {
        case "2.12" | "2.13" => "1.3.4"
      }
    }) ++ publishSettings ++ pomSettings)

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "org.scalatest" %% "scalatest" % "3.1.0" % "test"
)
