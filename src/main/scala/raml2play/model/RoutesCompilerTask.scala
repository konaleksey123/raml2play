package raml2play.model

import java.io.File

/**
 * A routes compiler task.
 *
 * @param file The routes file to compile.
 * @param additionalImports The additional imports.
 * @param forwardsRouter Whether a forwards router should be generated.
 */
case class RoutesCompilerTask(file: File, additionalImports: Seq[String], forwardsRouter: Boolean)

