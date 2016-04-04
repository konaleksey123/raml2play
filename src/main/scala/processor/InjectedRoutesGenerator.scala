package processor

import java.io.File

import play.routes.compiler.RoutesCompiler.RoutesCompilerTask
import play.routes.compiler._
import processor.RoutesGenerator
import processor.routes_file_parser.Rule

/**
 * @author vadim
 * @since 04.04.16
 */

/**
 * A routes generator that generates dependency injected routers
 */
object InjectedRoutesGenerator extends RoutesGenerator {

  import RoutesGenerator._

  case class Dependency[+T <: Rule](ident: String, clazz: String, rule: T)

  def generate(task: RoutesCompilerTask, namespace: Option[String], rules: List[Rule]): Seq[(String, String)] = {

    val folder = namespace.map(_.replace('.', '/') + "/").getOrElse("") + "/"

    val sourceInfo = RoutesSourceInfo(task.file.getCanonicalPath.replace(File.separator, "/"), new java.util.Date().toString)

    val forwardsRoutesFiles = if (task.forwardsRouter) {
      Seq(folder + ForwardsRoutesFile -> generateRouter(sourceInfo, namespace, task.additionalImports, rules))
    } else {
      Nil
    }

    val reverseRoutesFiles = if (task.reverseRouter) {
      Seq(folder + RoutesPrefixFile -> generateRoutesPrefix(sourceInfo, namespace)) ++
        generateReverseRouters(sourceInfo, namespace, task.additionalImports, rules, task.namespaceReverseRouter) ++
        generateJavaScriptReverseRouters(sourceInfo, namespace, task.additionalImports, rules, task.namespaceReverseRouter) ++
        generateJavaWrappers(sourceInfo, namespace, rules, task.namespaceReverseRouter)
    } else {
      Nil
    }

    forwardsRoutesFiles ++ reverseRoutesFiles
  }

  private def generateRouter(sourceInfo: RoutesSourceInfo, namespace: Option[String], additionalImports: Seq[String], rules: List[Rule]) = {

    // Generate dependency descriptors for all routes
    val routesDeps = rules
      .collect { case route: Route => route }
      .groupBy(r => (r.call.packageName, r.call.controller, r.call.instantiate))
      .zipWithIndex.map {
      case ((key @ (packageName, controller, instantiate), routes), index) =>
        val clazz = packageName + "." + controller
        // If it's using the @ syntax, we depend on the provider (ie, look it up each time)
        val dep = if (instantiate) s"javax.inject.Provider[$clazz]" else clazz
        val ident = controller + "_" + index
        key -> Dependency(ident, dep, routes.head)
    }.toMap

    // Get the distinct dependency descriptors in the same order as defined in the routes file
    val orderedDeps = rules.map {
      case route: Route =>
        routesDeps((route.call.packageName, route.call.controller, route.call.instantiate))
    }.distinct

    // Map all the rules to dependency descriptors
    val rulesWithDeps = rules.map {
      case route: Route =>
        routesDeps((route.call.packageName, route.call.controller, route.call.instantiate)).copy(rule = route)
    }

    inject.twirl.forwardsRouter(
      sourceInfo,
      namespace,
      additionalImports,
      orderedDeps,
      rulesWithDeps,
      includesDeps.values.toSeq
    ).body
  }

  private def generateRoutesPrefix(sourceInfo: RoutesSourceInfo, namespace: Option[String]) =
    static.twirl.routesPrefix(
      sourceInfo,
      namespace,
      _ => true
    ).body

  private def generateReverseRouters(sourceInfo: RoutesSourceInfo, namespace: Option[String], additionalImports: Seq[String], routes: List[Route], namespaceReverseRouter: Boolean) = {
    routes.groupBy(_.call.packageName).map {
      case (pn, routes) =>
        val packageName = namespace.filter(_ => namespaceReverseRouter).map(_ + "." + pn).getOrElse(pn)
        (packageName.replace(".", "/") + "/" + ReverseRoutesFile) ->
          static.twirl.reverseRouter(
            sourceInfo,
            namespace,
            additionalImports,
            packageName,
            routes,
            namespaceReverseRouter,
            _ => true
          ).body
    }
  }

  private def generateJavaScriptReverseRouters(sourceInfo: RoutesSourceInfo, namespace: Option[String], additionalImports: Seq[String], routes: List[Route], namespaceReverseRouter: Boolean) = {
    routes.groupBy(_.call.packageName).map {
      case (pn, routes) =>
        val packageName = namespace.filter(_ => namespaceReverseRouter).map(_ + "." + pn).getOrElse(pn)
        (packageName.replace(".", "/") + "/javascript/" + JavaScriptReverseRoutesFile) ->
          static.twirl.javascriptReverseRouter(
            sourceInfo,
            namespace,
            additionalImports,
            packageName,
            routes,
            namespaceReverseRouter,
            _ => true
          ).body
    }
  }

  private def generateJavaWrappers(sourceInfo: RoutesSourceInfo, namespace: Option[String], rules: List[Rule], namespaceReverseRouter: Boolean) = {
    rules.collect { case r: Route => r }.groupBy(_.call.packageName).map {
      case (pn, routes) =>
        val packageName = namespace.filter(_ => namespaceReverseRouter).map(_ + "." + pn).getOrElse(pn)
        val controllers = routes.groupBy(_.call.controller).keys.toSeq

        (packageName.replace(".", "/") + "/" + JavaWrapperFile) ->
          static.twirl.javaWrappers(sourceInfo, namespace, packageName, controllers).body
    }
  }
}
