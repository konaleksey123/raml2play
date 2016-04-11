package raml2play.model

/**
 * @author vadim
 * @since 05.04.16
 */

/**
 * An HTTP verb
 */
class HttpVerb(v: String) {
  val value = v.toUpperCase
  override def toString = v.toUpperCase
  override def equals(that: Any): Boolean = that match {
    case that: HttpVerb => that.value == value
    case _ => false
  }
  override def hashCode:Int = value.hashCode
}

object HttpVerb {
  def apply(v: String): HttpVerb = new HttpVerb(v)
}