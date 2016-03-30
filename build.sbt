sbtPlugin := true

name := "play-raml"

organization := "bavadim"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.10.4"

sbtVersion := "0.13.8"

libraryDependencies ++= Seq(
  "commons-io" % "commons-io" % "2.4",
  "com.typesafe.play" % "routes-compiler_2.10" % "2.4.3",
  "com.typesafe.play" %% "play" % "2.4.3",
  "org.raml" % "raml-parser" % "0.8.12",
  "org.scalatest" %% "scalatest" % "3.0.0-M12" % Test)

publishMavenStyle := false

lazy val root = (project in file(".")).settings(addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.4.3"))

