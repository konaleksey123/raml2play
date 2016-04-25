package raml2play.sbt

import play.sbt.{PlayScala, Play}
import raml2play.model.RoutesCompilerTask
import sbt.Keys._
import sbt._

import scala.language.postfixOps

object Raml2PlayPlugin extends AutoPlugin {
  lazy val ramlCompileTask = TaskKey[Seq[RoutesCompilerTask]]("raml-compile-task")
  lazy val routesImport = TaskKey[Seq[String]]("play-raml-routes-imports")

  def ramlCompileTasks(dirs: Seq[File], routesImport: Seq[String]): Seq[RoutesCompilerTask] = {
    (dirs * "api.raml").get.map { file =>
      RoutesCompilerTask(file, routesImport, forwardsRouter = true)
    }
  }

  override val requires = PlayScala
  override def trigger = allRequirements

  routesImport in Compile := Seq.empty

  override lazy val projectSettings = Seq(
      sourceGenerators in Compile += ((state, sourceManaged in Compile, unmanagedResourceDirectories in Compile) map {
        case (s, sm, dirs) =>
          raml2play.compiler.Compiler.compileRoutes(ramlCompileTasks(dirs, Seq()), sm, s.log)
    }).taskValue
  )
}
