logLevel := Level.Warn

addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat" % "0.1.16")
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.9.0")
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.5")
addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.0")

resolvers += "bondlink-maven-repo" at "https://raw.githubusercontent.com/mblink/maven-repo/main"
addSbtPlugin("bondlink" % "sbt-git-publish" % "0.0.4")
