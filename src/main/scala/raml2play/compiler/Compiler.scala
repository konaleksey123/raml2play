package raml2play.compiler

import java.io.File

import org.apache.commons.io.FileUtils
import raml2play.RoutesGenerator
import raml2play.model.{RoutesCompilationError, RoutesCompilerTask}
import raml2play.processor.RoutesFileParser
import sbt.Logger

import scala.io.Codec
import scala.util.{Failure, Success}

/**
 * @author vadim
 * @since 10.11.15
 */
object Compiler {
  def compileRoutes(tasks: Seq[RoutesCompilerTask], generatedDir: File, log: Logger): Seq[File] = {

    val (products, errors) = {
      val results = tasks.map { task => compile(task, generatedDir, "raml", log) }
      val (lefts, rights) = results.partition(_.isLeft)
      val errors = lefts.collect{ case Left(e) => e }.flatten
      val files = rights.collect{ case Right(r) => r }.flatten
      (files, errors)
    }

    if (errors.nonEmpty) {
      val exceptions = errors.map {
        case RoutesCompilationError(source, message, line, column) =>
          //TODO
          log.error(message)
          new RuntimeException(message)
      }
      throw exceptions.head
    }

    products.to[Seq]
  }

  def compile(task: RoutesCompilerTask, generatedDir: File,
              fileExtension: String, log: Logger): Either[Seq[RoutesCompilationError], Seq[File]] = {
    val namespace = Some("router")

    val routeFile = task.file.getAbsoluteFile
    RoutesFileParser.parseFile(routeFile).map { rules =>
      val generated = RoutesGenerator.generate(task, namespace, rules)
      generated.map {
        case (filename, content) =>
          val file = new File(generatedDir, filename)
          FileUtils.writeStringToFile(file, content, implicitly[Codec].name)
          log.info(s"Writing file: $filename")
          file
      }
    } match {
      case Success(r) => Right(r)
      case Failure(e) => Left(Seq(RoutesCompilationError(task.file, e.getMessage, None, None)))
    }
  }
}
