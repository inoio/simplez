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

sourceGenerators in Compile += Def.task {
  import SimplezGenerator._
  val result : Seq[(String, String)] = makeSomeSources()
  result.map{case (name, content) =>
    val file = (sourceManaged in Compile).value / "simplez" / name
    IO.write(file, content)
    file
  }
}.taskValue

testOptions in Test += Tests.Argument("console", "markdown")

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Managed

scalariformSettings

