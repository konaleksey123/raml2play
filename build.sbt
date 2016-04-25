import bintray.Keys._

lazy val commonSettings = Seq(
  version in ThisBuild := "0.2",
  organization in ThisBuild := "bavadim"
)

lazy val root = (project in file(".")).
  settings(commonSettings ++ bintrayPublishSettings: _*).
  settings(
    sbtPlugin := true,
    name := "raml2play",
    description := "Compile play routes from RAML file",
    homepage := some(url("https://github.com/bavadim/raml2play")),
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
    licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html")),
    repository in bintray := "sbt-raml-plugins",
    bintrayOrganization in bintray := None,
    libraryDependencies ++= Seq(
      "org.raml" % "raml-parser" % "0.8.12" exclude("org.slf4j", "slf4j-log4j12"),
      "org.scalatest" %% "scalatest" % "3.0.0-M12" % Test),
    addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.5.1")
  ).enablePlugins(SbtTwirl)
