name := "mbtree"

scalaVersion := "2.9.1"

scalacOptions ++= Seq(
"-optimize",
"-unchecked", 
"-deprecation"
)

libraryDependencies ++= Seq(
"org.scalatest" %% "scalatest" % "latest.integration" % "test"
)


