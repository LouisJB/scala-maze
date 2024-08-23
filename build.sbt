//val scalaVer = "2.12.18"
val scalaVer = "2.13.14"

lazy val root = project
  .in(file("."))
  .settings(
    name := "maze",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scalaVer,
    scalacOptions ++= Seq("-deprecation", "-feature"),
    libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
  )
