package raml2play.model

import java.time.ZonedDateTime

import scala.util.matching.Regex

/**
 * @author vadim
 * @since 31.03.16
 */

object Parameter {
  def paramType(p: Parameter): String = {
    def wrapToContainers(isOption: Boolean, isList: Boolean, typeName: String) = {
      def wrapToList(isList: Boolean, typeName: String) = if (isList) "List[" + typeName + "]" else typeName
      def wrapToOption(isList: Boolean, typeName: String) = if (isList) "Option[" + typeName + "]" else typeName
      wrapToOption(isOption, wrapToList(isList, typeName))
    }

    wrapToContainers(p.required, p.repeated, p match {
      case BooleanParameter(_, _, _, _) => "Boolean"
      case DateParameter(_, _, _, _) => "ZonedDateTime"
      case IntegerParameter(_, _, _, _, _, _) => "BigInt"
      case NumberParameter(_, _, _, _, _, _) => "BigDecimal"
      case StringParameter(_, _, _, _, _, _, _, _) => "String"
    })
  }
}

abstract class Parameter(val name: String, val repeated: Boolean, val required: Boolean, dflt: Option[String]) {
  override def toString = name + ":" + paramType + dflt.map(" ?= " + _).getOrElse("")

  private def wrapTypeToContainers(isOption: Boolean, isList: Boolean, typeName: String) = {
    def wrapToList(isList: Boolean, typeName: String) = if (isList) "List[" + typeName + "]" else typeName
    def wrapToOption(isRequired: Boolean, typeName: String) = if (!isRequired) "Option[" + typeName + "]" else typeName
    wrapToOption(isOption, wrapToList(isList, typeName))
  }

  private def wrapValToContainers(isOption: Boolean, isList: Boolean, value: String): String = {
    def wrapToList(isList: Boolean, value: String) = if (isList) "List(" + value + ")" else value
    def wrapToOption(isRequired: Boolean, value: String) = if (!isRequired) "Some(" + value + ")" else value
    wrapToOption(isOption, wrapToList(isList, value))
  }

  protected def rawParamType: String

  def paramType = wrapTypeToContainers(required, repeated, rawParamType)

  def default: Option[String] = dflt.map(wrapValToContainers(required, repeated, _))
}

case class StringParameter(override val name: String, override val repeated: Boolean, override val required: Boolean,
                           dflt: Option[String], pattern: Option[Regex], enum: Option[List[String]], minLength: Option[Int],
                           maxLength: Option[Int]) extends Parameter(name, repeated, required, dflt) {
  override def rawParamType = "String"

  override def default: Option[String] = super.default.map("\"" + _.toString  + "\"")

  override def equals(that: Any): Boolean = that match {
    case that: StringParameter => that.name == name && that.repeated == repeated && that.required == required &&
      that.dflt == dflt && that.pattern.map(_.toString()) == pattern.map(_.toString()) && that.enum == enum &&
      that.minLength == minLength && that.maxLength == maxLength
    case _ => false
  }
  override def hashCode:Int = name.hashCode + repeated.hashCode() + required.hashCode() + dflt.hashCode() +
    pattern.fold(0)(_.hashCode()) + enum.hashCode() + minLength.hashCode() + maxLength.hashCode()
}

case class NumberParameter(override val name: String, override val repeated: Boolean, override val required: Boolean,
                           dflt: Option[String], minimum: Option[BigDecimal],
                           maximum: Option[BigDecimal]) extends Parameter(name, repeated, required, dflt) {
  override def rawParamType = "BigDecimal"
}

case class IntegerParameter(override val name: String, override val repeated: Boolean, override val required: Boolean,
                            dflt: Option[String], minimum: Option[BigInt],
                            maximum: Option[BigInt]) extends Parameter(name, repeated, required, dflt) {
  override def rawParamType = "BigInt"
}

case class DateParameter(override val name: String, override val repeated: Boolean, override val required: Boolean,
                         dflt: Option[String]) extends Parameter(name, repeated, required, dflt) {
  override def rawParamType = "ZonedDateTime"

  override def default: Option[String] = super.default.map("ZonedDateTime.parse(" + _.toString  + ")")
}

case class BooleanParameter(override val name: String, override val repeated: Boolean, override val required: Boolean,
                            dflt: Option[String]) extends Parameter(name, repeated, required, dflt) {
  override def rawParamType = "Boolean"
}



