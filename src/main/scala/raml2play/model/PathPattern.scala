package raml2play.model

import scala.util.parsing.input.Positional

/**
 * @author vadim
 * @since 06.04.16
 */

/**
 * A part of the path
 */
trait PathPart

/**
 * A dynamic part, which gets extracted into a parameter.
 *
 * @param name The name of the parameter that this part of the path gets extracted into.
 * @param constraint The regular expression used to match this part.
 * @param encode Whether this part should be encoded or not.
 */
case class DynamicPart(name: String, constraint: String, encode: Boolean) extends PathPart with Positional {
  override def toString = """DynamicPart("""" + name + "\", \"\"\"" + constraint + "\"\"\"," + encode + ")" //"
}

/**
 * A static part of the path, which is matched as is.
 */
case class StaticPart(value: String) extends PathPart {
  override def toString = """StaticPart("""" + value + """")"""
}

case class PathPattern(parts: Seq[PathPart]) {

  /**
   * Whether this path pattern has a parameter by the given name.
   */
  def has(key: String): Boolean = parts.exists {
    case DynamicPart(name, _, _) if name == key => true
    case _ => false
  }

  override def toString = parts.map {
    case DynamicPart(name, constraint, encode) => "$" + name + "<" + constraint + ">"
    case StaticPart(path) => path
  }.mkString

}
