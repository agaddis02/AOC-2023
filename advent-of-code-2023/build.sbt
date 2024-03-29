val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code-2023",
    version := "0.1.0-SNAPSHOT",
    fork in run := true,
    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies ++= Seq(
        "org.scala-lang" %% "toolkit" % "0.2.0",
        "org.scala-lang" %% "toolkit-test" % "0.2.0" % Test
      )
  )
