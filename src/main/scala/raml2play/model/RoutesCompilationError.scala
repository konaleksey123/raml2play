package raml2play.model

import java.io.File

/**
 * A routes compilation error
 *
 * @param source The source of the error
 * @param message The error message
 * @param line The line that the error occurred on
 * @param column The column that the error occurred on
 */
case class RoutesCompilationError(source: File, message: String, line: Option[Int], column: Option[Int])
