package processor

import scala.language.postfixOps
import scala.util.parsing.combinator._

/**
 * @author vadim
 * @since 02.11.15
 */

//Based on daltontf/scala-yaml
class YAMLParser extends RegexParsers {
  override def skipWhitespace = false

  protected val mapSeparator = ": \r?\n?"r
  protected val seqIndicator = "- ?\r?\n?"r
  private val comment = """ *#[^\r\n]*"""
  private val commentLine = ("\n*" + comment + "\r*\n+").r
  protected def mappingKey = """[^-:\{\[\r\n][^-:\r\n]*"""r
  protected val newLine = commentLine | """ *\r*\n+""".r
  protected val inlineSeparator = " *,[ \r\n]+".r
  protected val mapBegin = """\{[ \n\r]*"""r
  protected val mapEnd = """[\n ]*\}"""r
  protected val listBegin = """\[[ \n\r]*"""r
  protected val listEnd = """[\n ]*]"""r

  protected def parseMapEntry(key: String, value: Any): Map[String, Any] = Map(key -> value)

  protected def leadingSpaces(numLeadingSpaces:Int): Parser[Int] =
    ("^[ ]{" + numLeadingSpaces + ",}"r) ^^ { _.length }

  protected def mappings(numLeadingSpaces: Int): Parser[Map[String, Any]] =
    indentedMap(numLeadingSpaces) | inlineMap(numLeadingSpaces)

  private def listOfMapsFlatten(map: List[Map[String, Any]]): Map[String, Any] =
    map.foldRight(Map[String, Any]()) { (m, acc) =>
      m ++ acc
    }

  protected def indentedMap(numLeadingSpaces: Int): Parser[Map[String, Any]] =
    rep1sep(indentedMapping(numLeadingSpaces), newLine) ^^ listOfMapsFlatten

  protected def inlineMap(numLeadingSpaces: Int): Parser[Map[String, Any]] = mapBegin ~>
    repsep(inlineMapping(numLeadingSpaces), inlineSeparator) <~ mapEnd ^^ listOfMapsFlatten

  protected def indentedMapping(numLeadingSpaces: Int): Parser[Map[String, Any]] =
    leadingSpaces(numLeadingSpaces) ~ mappingKey >> { case i ~ key => (
      ": +".r ~> (inlineMap(i + 1) | inlineList(i + 1) |
        scalar( """[^\r\n#][^\r\n#]*""", i + 1)) |
      ":" ~> newLine ~> (indentedList(i + 1) | indentedMap(i + 1))
      ) ^^ { case value => parseMapEntry(key, value) } }

  protected def inlineMapping(numLeadingSpaces: Int): Parser[Map[String, Any]] =
    mappingKey ~ mapSeparator ~ (inlineList(numLeadingSpaces + 1) | inlineMap(numLeadingSpaces + 1) |
      scalar("""[^,\r\n\}#]+""", numLeadingSpaces + 1)) ^^ { case key ~_ ~ value => parseMapEntry(key, value) }

  protected def list(numLeadingSpaces:Int): Parser[List[Any]] =
    (inlineList(numLeadingSpaces) | indentedList(numLeadingSpaces)) ^^ { List() ++ _ }

  protected def indentedList(numLeadingSpaces:Int): Parser[List[Any]] =
    rep1sep( leadingSpaces(numLeadingSpaces) <~ seqIndicator >> { i => nestedListData(i + 1) }, newLine)

  protected def inlineList(numLeadingSpaces: Int): Parser[List[Any]] = listBegin ~> repsep(nestedListData(0), inlineSeparator) <~ listEnd

  private def nestedListData(numLeadingSpaces: Int): Parser[Any] =
    list(numLeadingSpaces) | mappings(numLeadingSpaces) |
      (indentedMapping(0) ~ newLine ~ rep1sep(indentedMapping(numLeadingSpaces + 1) // + 1 from space after -
        , newLine) ^^ { case head ~ _ ~ tail => head ++ listOfMapsFlatten(tail)
      }) |
      scalar( """[^,\r\n\]\{\}\[#]+""", numLeadingSpaces + 1)

  private def blockScalar(numLeadingSpaces: Int): Parser[String] =
    ("|" | ">") ~ """[^\r\n]*""".r ~ """\r*\n+""".r ~ repsep(leadingSpaces(numLeadingSpaces) ~> """[^\r\n]*""".r, """\r*\n+""".r) ^^ {
      case "|" ~ first ~ _ ~ l => l.foldLeft(first)((acc, s) => acc + "\n" + s)
      case ">" ~ first ~ _ ~ l => l.foldLeft(first)((acc, s) => acc + " " + s).trim
    }

  private def inlineScalar(regexString: String) = regexString.r  ^^ { case value => value.trim }

  def scalar(regexString: String, numLeadingSpaces: Int): Parser[String] =  blockScalar(numLeadingSpaces) | inlineScalar(regexString)
  
  def yaml: Parser[Any] = namedError(opt(newLine) ~> (list(0) | mappings(0)), "Not YAML!")

  def parse(text : String): ParseResult[Any] = parse(yaml, text)


  def namedError[A](p: Parser[A], msg: String): Parser[A] = Parser[A] { i =>
    p(i) match {
      case Failure(_, in) => Failure(msg, in)
      case o => o
    }
  }
}
