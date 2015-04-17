val commonSettings = Seq(
  name := "simplez",
  organization := "inoio",
  version := "1.0.0-SNAPSHOT",
  scalacOptions ++= Seq(
    "-feature",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials",
    "-language:postfixOps",
    "-deprecation"
  ),
  scalaVersion := "2.11.6",
  autoCompilerPlugins := true,
  resolvers += "bintray/non" at "http://dl.bintray.com/non/maven",
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.5.2")
) ++ scalariformSettings


lazy val main = project.in(file("main"))
  .settings(commonSettings: _*)
  .settings(
  libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "2.4" % "test"
  ),
    testOptions in Test += Tests.Argument("console", "markdown"),
    sourceGenerators in Compile += Def.task {
      val result : Seq[(String, String)] = SimplezGenerator.makeSomeSources()
      result.map{case (name, content) =>
        val file = (sourceManaged in Compile).value / "simplez" / name
        IO.write(file, content)
        file
      }
    }.taskValue
  )


val exampleDependencies = Seq(
  "com.typesafe.play" % "play-json_2.11" % "2.4.0-M1",
  "org.typelevel" %% "shapeless-scalaz" % "0.3",
   "org.specs2" %% "specs2" % "2.4" % "test"
)

lazy val examples = project.in(file("examples"))
  .dependsOn(main)
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= exampleDependencies
  )
  .settings(name := "simplez-examplez")
