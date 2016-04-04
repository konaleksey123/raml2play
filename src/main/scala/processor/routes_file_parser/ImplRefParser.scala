package processor.routes_file_parser

import scala.util.Try
import scala.util.parsing.combinator.RegexParsers

/**
 * @author vadim
 * @since 01.04.16
 */
object ImplRefParser extends RegexParsers {
  private def call(verb: String, uri: String, arguments: Seq[Argument]): Parser[Try[Rule]] = {
    def methodNameParse(absMethod: List[String]) = {
      val (packageParts, classAndMethod) = absMethod.splitAt(absMethod.size - 2)
      val packageName = packageParts.mkString(".")
      val className = classAndMethod.head
      val methodName = classAndMethod(1)
      (packageName, className, methodName)
    }

    opt("@") ~ method ~ opt("(" ~ args ~ ")") ^^ {
      case instantiate ~ absMethod ~ None =>
        val dynamic = instantiate.isDefined
        val (packageName, className, methodName) = methodNameParse(absMethod)
        scala.util.Success(Rule(dynamic, verb, uri, packageName, className, methodName, Seq()))
      case instantiate ~ absMethod ~ Some( _ ~ params ~ _ ) =>
        val dynamic = instantiate.isDefined
        val (packageName, className, methodName) = methodNameParse(absMethod)

        val argumentsM = arguments.map (a => a.name -> a).toMap
        Try(params.map(argumentsM(_)))
          .map(Rule(dynamic, verb, uri, packageName, className, methodName, _))
          .recoverWith {
            case e: Exception => scala.util.Failure(new RuntimeException(s"Argument not defined", e))
          }
    }
  }

  private def method: Parser[List[String]] = namedError(javaIdent ~ "." ~ javaIdent ~ "." ~ rep1sep(javaIdent, ".") ^^ {
    case first ~ _ ~ second ~ _ ~ rest => first :: second :: rest
  }, "Controller method call expected")

  private def args: Parser[List[String]] = namedError(rep1sep(javaIdent, ",") ^^ {
    case rest => rest
  }, "Controller method parameters list expected")

  private def javaIdent: Parser[String] = """\p{javaJavaIdentifierStart}\p{javaJavaIdentifierPart}*""".r

  private def namedError[A](p: Parser[A], msg: String): Parser[A] = Parser[A] { i =>
    p(i) match {
      case Failure(_, in) => Failure(msg, in)
      case o => o
    }
  }

  def parse(str: String, verb: String, uri: String, arguments: Seq[Argument]): Try[Rule] =
    parse(call(verb, uri, arguments), str).get
}
