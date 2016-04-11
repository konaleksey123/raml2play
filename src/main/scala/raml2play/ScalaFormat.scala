package raml2play
import play.twirl.api.{ Format, BufferedContent }
import scala.collection.immutable

/**
 * @author vadim
 * @since 05.04.16
 */

/**
 * Twirl scala content type
 */
class ScalaContent(elements: immutable.Seq[ScalaContent], text: String) extends BufferedContent[ScalaContent](elements, text) {
  def this(text: String) = this(Nil, text)
  def this(elements: immutable.Seq[ScalaContent]) = this(elements, "")

  def contentType = "application/scala"
}

/**
 * Twirl Scala format
 */
object ScalaFormat extends Format[ScalaContent] {
  def raw(text: String) = new ScalaContent(text)

  def escape(text: String) = new ScalaContent(text)

  val empty = new ScalaContent(Nil)

  def fill(elements: immutable.Seq[ScalaContent]) = new ScalaContent(elements)
}

