name := "dna-analysis"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "org.scalanlp" % "breeze_2.11" % "0.12"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"

//libraryDependencies += "org.scalanlp" %% "breeze-viz_2.11" % "0.12"

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"