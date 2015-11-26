package processor

import java.io.File

import play2raml.routes.compiler.RouteParser
import play.routes.compiler._

import scala.language.postfixOps

/**
 * @author vadim
 * @since 02.11.15
 */
class RAMLRouteParser extends YAMLParser with RouteParser {
  protected def httpVerbs: Parser[String] = "get" | "post" | "put" | "delete" | "head" | "patch" | "options" | "GET" | "POST" | "PUT" | "PATCH" | "HEAD" | "DELETE" | "OPTIONS"

  private def isVerb(v: String) = v.toLowerCase == "get" | v.toLowerCase ==  "post" | v.toLowerCase ==  "put" |
    v.toLowerCase ==  "delete" | v.toLowerCase ==  "head" | v.toLowerCase ==  "patch" | v.toLowerCase ==  "options"

  protected override def parseMapEntry(key: String, value: Any): Map[String, Any] = {
    def call: Parser[HandlerCall] = newLine ~> opt("@") ~ absoluteMethod ~ opt(parameters) <~ repsep("""[^\r\n]*"""r, newLine) ^^ {
      case instantiate ~ absMethod ~ parameters =>
        val (packageParts, classAndMethod) = absMethod.splitAt(absMethod.size - 2)
        val packageName = packageParts.mkString(".")
        val className = classAndMethod(0)
        val methodName = classAndMethod(1)
        val dynamic = instantiate.isDefined
        HandlerCall(packageName, className, dynamic, methodName, parameters)
    }

    def expression: Parser[String] = (multiString | string | parentheses | brackets | """[^),?=\n]""".r).+ ^^ {
      case p => p.mkString
    }

    def parameterFixedValue: Parser[String] = "=" ~ ignoreWhiteSpace ~ expression ^^ {
      case a ~ _ ~ b => a + b
    }

    def parameterDefaultValue: Parser[String] = "?=" ~ ignoreWhiteSpace ~ expression ^^ {
      case a ~ _ ~ b => a + b
    }

    def parameterType: Parser[String] = ":" ~> ignoreWhiteSpace ~> simpleType

    def simpleType: Parser[String] = {
      ((stableId <~ ignoreWhiteSpace) ~ opt(typeArgs)) ^^ {
        case sid ~ ta => sid.toString + ta.getOrElse("")
      } |
        (space("(") ~ types ~ space(")")) ^^ {
          case _ ~ b ~ _ => "(" + b + ")"
        }
    }

    def typeArgs: Parser[String] = {
      (space("[") ~ types ~ space("]") ~ opt(typeArgs)) ^^ {
        case _ ~ ts ~ _ ~ ta => "[" + ts + "]" + ta.getOrElse("")
      } |
        (space("#") ~ identifier ~ opt(typeArgs)) ^^ {
          case _ ~ id ~ ta => "#" + id + ta.getOrElse("")
        }
    }

    def types: Parser[String] = rep1sep(simpleType, space(",")) ^^ (_ mkString ",")

    def stableId: Parser[String] = rep1sep(identifier, space(".")) ^^ (_ mkString ".")

    def parameter: Parser[Parameter] = ((identifier | tickedIdentifier) <~ ignoreWhiteSpace) ~ opt(parameterType) ~ (ignoreWhiteSpace ~> opt(parameterDefaultValue | parameterFixedValue)) ^^ {
      case name ~ t ~ d => Parameter(name, t.getOrElse("String"), d.filter(_.startsWith("=")).map(_.drop(1)), d.filter(_.startsWith("?")).map(_.drop(2)))
    }

    def parameters: Parser[List[Parameter]] = "(" ~> repsep(ignoreWhiteSpace ~> positioned(parameter) <~ ignoreWhiteSpace, ",") <~ ")"

    // Absolute method consists of a series of Java identifiers representing the package name, controller and method.
    // Since the Scala parser is greedy, we can't easily extract this out, so just parse at least 3
    def absoluteMethod: Parser[List[String]] = namedError(javaIdent ~ "." ~ javaIdent ~ "." ~ rep1sep(javaIdent, ".") ^^ {
      case first ~ _ ~ second ~ _ ~ rest => first :: second :: rest
    }, "Controller method call expected")

    def path: Parser[PathPattern] = {
      def singleComponentPathPart: Parser[DynamicPart] = ("{" ~> identifier <~ "}") ^^ {
        case name => DynamicPart(name, """[^/]+""", encode = true)
      }

      def staticPathPart: Parser[StaticPart] = ("""[\w]""".r +) ^^ {
        case chars => StaticPart((chars :+ "/").mkString)
      }

      "/" ~> rep1sep(singleComponentPathPart | staticPathPart, "/") ^^ PathPattern
    }

    def several[T](p: => Parser[T]): Parser[List[T]] = Parser { in =>
      import scala.collection.mutable.ListBuffer
      val elems = new ListBuffer[T]
      def continue(in: Input): ParseResult[List[T]] = {
        val p0 = p // avoid repeatedly re-evaluating by-name parser
        @scala.annotation.tailrec
        def applyp(in0: Input): ParseResult[List[T]] = p0(in0) match {
            case Success(x, rest) =>
              elems += x; applyp(rest)
            case Failure(_, _) => Success(elems.toList, in0)
            case err: Error => err
          }
        applyp(in)
      }
      continue(in)
    }

    def ignoreWhiteSpace: Parser[Option[String]] = opt(whiteSpace)

    // This won't be needed when we upgrade to Scala 2.11, we will then be able to use JavaTokenParser.ident:
    // https://github.com/scala/scala/pull/1466
    def javaIdent: Parser[String] =
      """\p{javaJavaIdentifierStart}\p{javaJavaIdentifierPart}*""".r

    def tickedIdent: Parser[String] = """`[^`]+`""".r

    def identifier: Parser[String] = namedError(javaIdent, "Identifier expected")

    def tickedIdentifier: Parser[String] = namedError(tickedIdent, "Identifier expected")

    def newLine: Parser[String] = namedError(("\r" ?) ~> "\n", "End of line expected")

    def blankLine: Parser[Unit] = ignoreWhiteSpace ~> newLine ^^ { case _ => () }

    def parentheses: Parser[String] = {
      "(" ~ several(parentheses | not(")") ~> """.""".r) ~ commit(")") ^^ {
        case p1 ~ charList ~ p2 => p1 + charList.mkString + p2
      }
    }

    def brackets: Parser[String] = {
      "[" ~ several(parentheses | not("]") ~> """.""".r) ~ commit("]") ^^ {
        case p1 ~ charList ~ p2 => p1 + charList.mkString + p2
      }
    }

    def string: Parser[String] = {
      "\"" ~ several(parentheses | not("\"") ~> """.""".r) ~ commit("\"") ^^ {
        case p1 ~ charList ~ p2 => p1 + charList.mkString + p2
      }
    }

    def multiString: Parser[String] = {
      "\"\"\"" ~ several(parentheses | not("\"\"\"") ~> """.""".r) ~ commit("\"\"\"") ^^ {
        case p1 ~ charList ~ p2 => p1 + charList.mkString + p2
      }
    }

    def space(s: String): Parser[String] = ignoreWhiteSpace ~> s <~ ignoreWhiteSpace

    def parseMethod(pref: String, body: Map[String, Any]): Seq[Route] = {
      body.collect {
        case (v: String, hc: HandlerCall) if isVerb(v) => Seq(Route(HttpVerb(v.toUpperCase), parse(path, pref).get, hc))
        case (s: String, l: Seq[Route]) if s.startsWith("/") =>
          val p1 = parse(path, pref).get.parts
          l.map { r =>
            val p2 = r.path.parts
            (p1.last, p2.head) match {
              case (d1: DynamicPart, d2: StaticPart) => r.copy(path = PathPattern((p1 :+ d2.copy("/" + d2.value)) ++ p2.tail))
              case _ => r.copy(path = PathPattern(p1 ++ p2))
            }
          }
      }.flatten.toSeq
    }

    (key, value) match {
      case ("description", v: String) =>
        if(parse(call, v.toString).isEmpty) Map()
        else Map("HC" -> parse(call, v.toString).get)
      case (v: String, m: Map[String, Any]) if isVerb(v) => m.collect { case ("HC", hc: HandlerCall) => Map(v -> hc) }.headOption.getOrElse(Map())
      case (s: String, m: Map[String, Any]) if s.startsWith("/") => Map(s -> parseMethod(s, m))
      case _ => Map()
    }
  }

  protected def raml: Parser[List[Rule]] = yaml ^^ { case m: Map[String, List[Route]] =>
    m.flatMap {
      case (_, l: List[Route]) => l.map { i =>
        i.copy(path = PathPattern(i.path.parts.dropRight(1) :+ (i.path.parts.last match {
          case StaticPart(v) => StaticPart(v.dropRight(1))
          case e => e
        })))
      }
    }.toList
  }

  def parseRAML(text : String) = parse(raml, text)

  override protected def parseContent(routesContent: String, file: File): Either[Seq[RoutesCompilationError], List[Rule]] =
    parseRAML(routesContent) match {
     case Success(l, _) => Right(l)
     case Failure(msg, _) => Left(Seq(RoutesCompilationError(file, msg, None, None)))
     case Error(msg, _) => Left(Seq(RoutesCompilationError(file, msg, None, None)))
   }
}
