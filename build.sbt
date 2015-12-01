sbtPlugin := true

name := "sbt-play2-raml"

organization := "ru.raiffeisen.elbrus"

version := "0.1.0-RELEASE"

scalaVersion := "2.10.4"

sbtVersion := "0.13.8"

libraryDependencies ++= Seq(
  "commons-io" % "commons-io" % "2.4",
  "com.typesafe.play" % "routes-compiler_2.10" % "2.4.3" ,
  "com.typesafe.play" %% "play" % "2.4.3",
  "org.scalatest" %% "scalatest" % "3.0.0-M11" % Test)

publishMavenStyle := false

lazy val root = (project in file(".")).settings(addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.4.3"))

publishTo <<= version { v: String =>
  val nexus = "http://10.242.144.2:8181/nexus/" 
  if (v.trim.endsWith("SNAPSHOT"))
    Some("Sonatype Nexus Repository Manager" at nexus + "content/repositories/snapshots")
  else
    Some("Sonatype Nexus Repository Manager" at nexus + "content/repositories/releases")
  }
credentials += Credentials(Path.userHome / ".sbt" / ".credentials")