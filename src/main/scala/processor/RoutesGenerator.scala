package processor

import play.routes.compiler.RoutesCompiler.RoutesCompilerTask
import processor.routes_file_parser.Rule

/**
 * @author vadim
 * @since 04.04.16
 */
trait RoutesGenerator {
  /**
   * Generate a router
   *
   * @param task The routes compile task
   * @param namespace The namespace of the router
   * @param rules The routing rules
   * @return A sequence of output filenames to file contents
   */
  def generate(task: RoutesCompilerTask, namespace: Option[String], rules: List[Rule]): Seq[(String, String)]
}

private object RoutesGenerator {
  val ForwardsRoutesFile = "Routes.scala"
  val ReverseRoutesFile = "ReverseRoutes.scala"
  val JavaScriptReverseRoutesFile = "JavaScriptReverseRoutes.scala"
  val RoutesPrefixFile = "RoutesPrefix.scala"
  val JavaWrapperFile = "routes.java"
}
