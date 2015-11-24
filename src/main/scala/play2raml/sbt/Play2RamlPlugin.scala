package play2raml.sbt

import java.io.File

import play2raml.routes
import play.routes.compiler.RoutesCompiler.RoutesCompilerTask
import play.sbt.PlayScala
import play.sbt.routes.{RoutesCompiler, _}
import sbt.Keys._
import sbt._
import play.twirl.sbt.Import.TwirlKeys

import scala.language.postfixOps


object Play2RamlPlugin extends AutoPlugin {

  override def requires = PlayScala

  override def trigger = allRequirements

  object autoImport {
    lazy val testRoutesFilePath = SettingKey[String]("test-routes-path", "Path to routes file during testing")
    lazy val ramlList = TaskKey[Seq[RoutesCompilerTask]]("playRamlRoutesTasks", "The raml routes files to compile")
  }

  import autoImport._

  override lazy val projectSettings = RoutesCompiler.defaultSettings ++
    inConfig(Compile)(routesSettings) ++
    inConfig(Test)(routesSettings)

  def routesSettings = Seq(
    testRoutesFilePath := "conf/api.raml",
    //redirect play generators
    ramlList <<= Def.taskDyn {
      // Aggregate all the routes file tasks that we want to compile the reverse routers for.
      RoutesKeys.aggregateReverseRoutes.value.map {
        agg => RoutesKeys.routesCompilerTasks in(agg.project, configuration.value)
      }.join.map { aggTasks: Seq[Seq[RoutesCompilerTask]] =>

        // Aggregated tasks need to have forwards router compilation disabled and reverse router compilation enabled.
        val reverseRouterTasks = aggTasks.flatten.map { task =>
          task.copy(forwardsRouter = false, reverseRouter = true)
        }

        def routesSources = {
          val dirs = (unmanagedResourceDirectories in Compile).value
          (dirs * "raml").get ++ (dirs * "*.raml").get
        }

        val thisProjectTasks = routesSources.map { file =>
          RoutesCompilerTask(file, RoutesKeys.routesImport.value, forwardsRouter = true,
            reverseRouter = RoutesKeys.generateReverseRouter.value, namespaceReverseRouter = RoutesKeys.namespaceReverseRouter.value)
        }

        thisProjectTasks ++ reverseRouterTasks
      }
    },
    sourceGenerators <+= Def.task[Seq[File]] {
        routes.compiler.Compiler.compileRoutes(ramlList.value,
          RoutesKeys.routesGenerator.value,
        (target in RoutesKeys.routes).value,
        streams.value.cacheDirectory,
        state.value.log)
      },
    doc <<= (doc in Compile) map { f =>
      val fl = s"${f.getAbsolutePath}/api.html"
      s"raml2html conf/api.raml -o ${fl}" !

      new java.io.File(fl)
    }
  )

}
