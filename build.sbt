val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "KATch",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "com.lihaoyi" %% "fastparse" % "3.0.2"
    )
  )

// addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "3.7.12")

// addSbtPlugin("org.scala-sbt" % "sbt-worksheet" %
