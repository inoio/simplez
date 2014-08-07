val commonSettings = Seq(
  name := "simplez",
  organization := "inoio",
  version := "1.0.0-SNAPSHOT",
  scalacOptions ++= Seq("-feature", "-deprecation"),
  crossScalaVersions := Seq("2.11.1", "2.10.4"),
  scalaVersion := "2.11.1"
) ++ scalariformSettings

lazy val main = project.in(file("main"))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies += "org.specs2" %% "specs2" % "2.3.12" % "test",
    scalacOptions in (Compile,doc) := Seq("-groups", "-implicits", "-doc-root-content", "rootdoc.txt"),
    testOptions in Test += Tests.Argument("console", "markdown"),
    EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Managed,
    sourceGenerators in Compile += Def.task {
      val result : Seq[(String, String)] = SimplezGenerator.makeSomeSources()
      result.map{case (name, content) =>
        val file = (sourceManaged in Compile).value / "simplez" / name
        IO.write(file, content)
        file
      }
    }.taskValue
  )

lazy val examples = project.in(file("examples"))
  .dependsOn(main)
  .settings(commonSettings: _*)
  .settings(name := "simplez-examplez")
