package raml2play.processor

import raml2play.model._

import scala.language.postfixOps
import scala.util.Try
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/**
 * @author vadim
 * @since 01.04.16
 */
object RefParser extends RegexParsers {
  private def call(verb: HttpVerb, uri: PathPattern, arguments: Seq[Parameter]): Parser[Try[Rule]] = {
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
        scala.util.Success(Rule(verb, uri, HandlerCall(packageName, className, dynamic, methodName, None)))
      case instantiate ~ absMethod ~ Some( _ ~ params ~ _ ) =>
        val dynamic = instantiate.isDefined
        val (packageName, className, methodName) = methodNameParse(absMethod)

        val argumentsM = arguments.map (a => a.name -> a).toMap
        Try(params.map(argumentsM(_)))
          .map(p => Rule(verb, uri, HandlerCall(packageName, className, dynamic, methodName, Some(p))))
          .recoverWith {
            case e: Exception => scala.util.Failure(new RuntimeException(s"Argument not defined", e))
          }
    }
  }

  def uri: Parser[PathPattern] = {
    def identifier: Parser[String] = namedError(javaIdent, "Identifier expected")
    
    def singleComponentPathPart: Parser[DynamicPart] = ("{" ~> identifier <~ "}") ^^ {
      case name => DynamicPart(name, """[^/]+""", encode = true)
    }

    /*def staticPathPart: Parser[StaticPart] = ("""[\w]""".r +) ^^ {
      case chars => StaticPart(chars.mkString)
    }

    rep("/") ~> opt(rep1sep(singleComponentPathPart | staticPathPart, "/")) ^^ {
      case Some(p) => PathPattern(p)
      case None => PathPattern(Seq(StaticPart("/")))
    }*/


    def staticPathPart: Parser[StaticPart] = (not("$") ~> not("{") ~> not("}") ~> """[^\s]""".r +) ^^ {
      case chars => StaticPart(chars.mkString)
    }

    "/" ~ ((positioned(singleComponentPathPart) | staticPathPart) *) ^^ {
      case _ ~ parts => PathPattern(parts)
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

  private def setRegexToQueryParameters(path: PathPattern, arguments: Seq[Parameter]): PathPattern = {
    val patterns = arguments.collect {
      case StringParameter(name, _, _, _, Some(pattern), enum, _, _) => name -> pattern
    }.toMap

    val parts = path.parts.map {
      case DynamicPart(name, regex, e) if patterns.contains(name) =>
        val pattern = patterns.get(name).get.toString()
        DynamicPart(name, pattern, e)
      case p => p
    }

    PathPattern(parts)
  }

  def parse(str: String, verb: String, uriStr: String, arguments: Seq[Parameter]): Try[Rule] =
    parse(uri, uriStr).map { pp =>
      parse(call(HttpVerb(verb), setRegexToQueryParameters(pp, arguments), arguments), str).get
    }.get
}
