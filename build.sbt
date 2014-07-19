name := "simplez"

organization := "inoio"

scalacOptions ++= Seq("-feature", "-deprecation")

crossScalaVersions := Seq("2.11.1", "2.10.4")

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "2.3.12" % "test"
)

scalacOptions in (Compile,doc) := Seq("-groups", "-implicits", 
  "-doc-root-content", "rootdoc.txt")
