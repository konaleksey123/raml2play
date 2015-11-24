package play2raml.routes.compiler

import java.io.File

import org.apache.commons.io.FileUtils
import play.api.PlayException
import play.routes.compiler.RoutesCompiler.RoutesCompilerTask
import play.routes.compiler.{RoutesCompilationError, RoutesGenerator}
import play.sbt.routes.RoutesCompilationException
import processor.RAMLRouteParser
import sbt.Logger

import scala.io.Codec

/**
 * @author vadim
 * @since 10.11.15
 */
object Compiler {
  def compileRoutes(tasks: Seq[RoutesCompilerTask], generator: RoutesGenerator, generatedDir: File,
                    cacheDirectory: File, log: Logger): Seq[File] = {

    val (products, errors) = {
      val results = tasks.map { task =>
        compile(task, generator, generatedDir, "raml", new RAMLRouteParser(), log)
      }
      val (lefts, rights) = results.partition(_.isLeft)
      val errors = lefts.flatMap { case Left(e) => e }
      val files = rights.flatMap { case Right(r) => r }
      (files, errors)
    }

    if (errors.nonEmpty) {
      val exceptions = errors.map {
        case RoutesCompilationError(source, message, line, column) =>
          reportCompilationError(log, RoutesCompilationException(source, message, line, column.map(_ - 1)))
      }
      throw exceptions.head
    }

    products.to[Seq]
  }

  def compile(task: RoutesCompilerTask, generator: RoutesGenerator, generatedDir: File,
              fileExtension: String, parser: RouteParser, log: Logger): Either[Seq[RoutesCompilationError], Seq[File]] = {

    //val namespace = Option(task.file.getName).filter(_.endsWith("." + fileExtension)).map(
    //  _.dropRight(("." + fileExtension).length))
    val namespace = Some("router")

    val routeFile = task.file.getAbsoluteFile
    val parsed = parser.parse(routeFile)
    parsed.right.map { rules =>
      val generated = generator.generate(task, namespace, rules)
      generated.map {
        case (filename, content) =>
          val file = new File(generatedDir, filename)
          FileUtils.writeStringToFile(file, content, implicitly[Codec].name)
          log.info(s"Writing file: $filename")
          file
      }
    }
  }

  private def reportCompilationError(log: Logger, error: PlayException.ExceptionSource) = {
    // log the source file and line number with the error message
    log.error(Option(error.sourceName).getOrElse("") + Option(error.line).map(":" + _).getOrElse("") + ": " + error.getMessage)
    Option(error.interestingLines(0)).map(_.focus).flatMap(_.headOption) foreach { line =>
      // log the line
      log.error(line)
      Option(error.position).foreach { pos =>
        // print a carat under the offending character
        val spaces = (line: Seq[Char]).take(pos).map {
          case '\t' => '\t'
          case x => ' '
        }
        log.error(spaces.mkString + "^")
      }
    }
    error
  }
}
