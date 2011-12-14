name := "mbtree"

scalaVersion := "2.9.1"

scalacOptions ++= Seq(
"-optimize",
"-unchecked", 
"-deprecation"
)

resolvers ++= Seq(
"Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
//"ScalaNLP Maven2" at "http://repo.scalanlp.org/repo",
"NativeLibs4Java Respository" at "http://nativelibs4java.sourceforge.net/maven/",
"mhendred github" at "https://github.com/mhendred/face4j/tree/master/face4j-core/src/main/java",
"twitter.com" at "http://maven.twttr.com"
)

//"org.scalala" % "scalala_2.9.0" % "1.0.0.RC2-SNAPSHOT",
//"org.scala-tools.testing" %% "scalacheck" % "1.9" % "test",
//"com.h2database" % "h2" % "1.2.140",
//"com.nativelibs4java" % "scalacl" % "0.2",
//"com.nativelibs4java" % "javacl" % "1.0.0-RC1",
//"com.nativelibs4java" % "javacl-generator" % "1.0.0-RC1",
//"com.github.mhendred.face4j" % "face4j-core" % "latest.integration",
//"org.scalaquery" % "scalaquery_2.9.0-1" % "latest.integration",
//"org.xerial" % "sqlite-jdbc" % "latest.integration",
//"com.twitter" % "querulous" % "latest.integration"

libraryDependencies ++= Seq(
"com.frugalmechanic" % "scala-optparse" % "latest.integration",
"org.scalala" %% "scalala" % "1.0.0.RC2-SNAPSHOT",
"org.scalatest" %% "scalatest" % "latest.integration",
"com.github.scala-incubator.io" %% "scala-io-core" % "latest.integration",
"com.github.scala-incubator.io" %% "scala-io-file" % "latest.integration"
)

// This is a bug workaround
libraryDependencies ++= Seq(
"org.antlr" % "stringtemplate" % "3.2.1"
)

fork in run := true
