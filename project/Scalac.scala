// compiler options
import sbt.Keys._
import sbt._

object Scalac {
  lazy val settings = Seq(
    scalacOptions ++= {
      val opts = Seq(
        "-deprecation",
        "-encoding", "UTF-8",
        "-explaintypes",
        "-feature",
        "-g:vars",
        "-target:jvm-1.8",
        "-unchecked",
        "-Xcheckinit",
        "-Xfatal-warnings",
        "-Xlint",
        "-Ywarn-numeric-widen",
        "-Ywarn-dead-code",
        "-Ywarn-value-discard",
        "-Yno-adapted-args",
        "-Ywarn-inaccessible",
        "-Ywarn-nullary-override",
        "-Ywarn-nullary-unit",
      )

      if (scalaBinaryVersion.value == "2.12") {
        opts ++ Seq(
          "-Ywarn-extra-implicit",
          "-Ywarn-infer-any",
          "-Ywarn-macros:after",
          "-Ywarn-unused:_",
        )
      } else {
        opts
      }
    },
    scalacOptions in (Compile, console) ~= {
      _.filterNot { opt => opt.startsWith("-X") || opt.startsWith("-Y") }
    },
    scalacOptions in (Test, console) ~= {
      _.filterNot { opt => opt.startsWith("-X") || opt.startsWith("-Y") }
    }
  )
}
