lazy val root = (project in file("."))
  .aggregate(spray)

lazy val commonSettings = Seq(
  libraryDependencies ++= testlibs("org.scalatest" %% "scalatest" % "3.0.0-M15")
)

lazy val sprayVersion = "1.3.3"

// ~ Exercise Definitions
lazy val spray = (project in file("spray"))
  .settings(
    name := "quickroutes-spray"
  )
  .settings(commonSettings: _*)
  .settings(
    resolvers += "spray repo" at "http://repo.spray.io"
  )
  .settings(libraryDependencies <++= (scalaVersion)(scalaVersion =>
    compilelibs(
      "io.spray" %% "spray-routing" % sprayVersion,
      "org.scala-lang" % "scala-compiler" % scalaVersion,
      "org.scalamacros" % "resetallattrs_2.11" % "1.0.0",
      "org.spire-math" %% "cats" % "0.3.0"
    ) ++
    testlibs(
      "io.spray" %% "spray-testkit" % sprayVersion
    )
  ))
