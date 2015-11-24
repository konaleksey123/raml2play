sbtPlugin := true

name := "sbt-play2-raml"

organization := "ru.raiffeisen"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.10.4"

sbtVersion := "0.13.8"

libraryDependencies ++= Seq(
  "commons-io" % "commons-io" % "2.4",
  "com.typesafe.play" % "routes-compiler_2.10" % "2.4.3" ,
  "com.typesafe.play" %% "play" % "2.4.3",
  "org.scalatest" %% "scalatest" % "3.0.0-M11" % Test)


publishMavenStyle := false

lazy val root = (project in file(".")).settings(addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.4.3"))

publishTo := Some("Local Artifactory" at "http://s-msk00-web02.raiffeisen.ru:9191/artifactory/repo")

credentials += Credentials(Path.userHome / ".sbt" / ".credentials")
