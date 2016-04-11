package raml2play

import java.io.File

import raml2play.model.{RoutesCompilerTask, Dependency, RoutesSourceInfo, Rule}

/**
 * @author vadim
 * @since 05.04.16
 */
private object RoutesGenerator {
  val ForwardsRoutesFile = "Routes.scala"
  val ReverseRoutesFile = "ReverseRoutes.scala"
  val JavaScriptReverseRoutesFile = "JavaScriptReverseRoutes.scala"
  val RoutesPrefixFile = "RoutesPrefix.scala"
  val JavaWrapperFile = "routes.java"

  /**
   * Information about the routes source file
   */
  def generate(task: RoutesCompilerTask, namespace: Option[String], rules: List[Rule]): Seq[(String, String)] = {

    val folder = namespace.map(_.replace('.', '/') + "/").getOrElse("") + "/"

    val sourceInfo = RoutesSourceInfo(task.file.getCanonicalPath.replace(File.separator, "/"), new java.util.Date().toString)

    val reverseRoutesFiles = {
      Seq(folder + RoutesPrefixFile -> generateRoutesPrefix(sourceInfo, namespace))
    }

    val forwardRoutesFiles = if (task.forwardsRouter) {
      Seq(folder + ForwardsRoutesFile -> generateRouter(sourceInfo, namespace, task.additionalImports, rules))
    } else {
      Nil
    }

    reverseRoutesFiles ++ forwardRoutesFiles
  }

  def generateRouter(sourceInfo: RoutesSourceInfo, namespace: Option[String], additionalImports: Seq[String],
                             rules: List[Rule]): String = {
    // Generate dependency descriptors for all routes
    val routesDeps = rules
      .groupBy(r => (r.call.packageName, r.call.controller, r.call.instantiate))
      .zipWithIndex.map {
      case ((key @ (packageName, controller, instantiate), routes), index) =>
        val clazz = packageName + "." + controller
        // If it's using the @ syntax, we depend on the provider (ie, look it up each time)
        val dep = if (instantiate) s"javax.inject.Provider[$clazz]" else clazz
        val ident = controller + "_" + index
        key -> Dependency(ident, dep, routes.head)
    }

    // Get the distinct dependency descriptors in the same order as defined in the routes file
    val orderedDeps = rules.map {
      case route: Rule => routesDeps((route.call.packageName, route.call.controller, route.call.instantiate))
    }.distinct

    // Map all the rules to dependency descriptors
    val rulesWithDeps = rules.map {
      case route: Rule => routesDeps((route.call.packageName, route.call.controller, route.call.instantiate)).copy(rule = route)
    }

    inject.txt.forwardsRouter(
      sourceInfo,
      namespace,
      additionalImports,
      orderedDeps,
      rulesWithDeps
    ).body
  }

  private def generateRoutesPrefix(sourceInfo: RoutesSourceInfo, namespace: Option[String]) =
    static.txt.routesPrefix(
      sourceInfo,
      namespace,
      _ => true
    ).body

  /*private def generateReverseRouters(sourceInfo: RoutesSourceInfo, namespace: Option[String], additionalImports: Seq[String], routes: List[Route], namespaceReverseRouter: Boolean) = {
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
  }*/
}
