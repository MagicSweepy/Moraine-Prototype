ThisBuild / organization := "com.morphismmc"
ThisBuild / version      := "1.0.0"
ThisBuild / scalaVersion  := "3.8.3"
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Wunused:all",
  "-Xfatal-warnings",
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "Moraine",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "ujson" % "4.1.0",
    ),
  )
