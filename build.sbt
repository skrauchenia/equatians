ThisBuild / scalaVersion := "3.3.6"
ThisBuild / organization := "equatians"

lazy val root = (project in file("."))
  .settings(
    name := "equatians",
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked"),
    libraryDependencies ++= Seq(
      "com.typesafe" % "config" % "1.4.3",
      "org.scilab.forge" % "jlatexmath" % "1.0.7",
      "org.scalameta" %% "munit" % "1.1.0" % Test
    ),
    Compile / mainClass := Some("equatians.ui.Main"),
    // Swing-окно запускаем в отдельной JVM, чтобы закрытие окна не роняло sbt.
    run / fork := true
  )
