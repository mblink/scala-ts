import sbt.Keys._

Global / onChangedBuildSource := ReloadOnSourceChanges

val publishSettings = Seq(
  publish / skip := false,
  publishMavenStyle := true,
  Test / publishArtifact := false,
  gitPublishDir := file("/src/maven-repo"),
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

lazy val scalaVersions = Seq("2.12.15", "2.13.8")

lazy val root = (project in file(".")).
  settings(Seq(
    name := "scala-ts",
    organization := "com.github.miloszpp",
    crossScalaVersions := scalaVersions,
    scalaVersion := scalaVersions.find(_.startsWith("2.13")).get,
  ) ++ publishSettings ++ pomSettings)

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "org.scalatest" %% "scalatest" % "3.1.0" % "test",
  "org.typelevel" %% "cats-core" % "2.7.0",
  // Scalaz is just needed for trying to cast values to types like `\/` and `\&/`
  "org.scalaz" %% "scalaz-core" % "7.3.6",
)
