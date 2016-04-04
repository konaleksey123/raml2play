package processor.routes_file_parser

import java.time.temporal.TemporalAccessor

/**
 * @author vadim
 * @since 31.03.16
 */

abstract class Argument(val name: String, val repeated: Boolean, val required: Boolean)

case class StringArgument(override val name: String, override val repeated: Boolean, override val required: Boolean,
                          default: Option[String], pattern: Option[String], enum: Option[List[String]], minLength: Option[Int],
                          maxLength: Option[Int]) extends Argument(name, repeated, required)

case class NumberArgument(override val name: String, override val repeated: Boolean, override val required: Boolean,
                          default: Option[BigDecimal], minimum: Option[BigDecimal], maximum: Option[BigDecimal]) extends Argument(name, repeated, required)

case class IntegerArgument(override val name: String, override val repeated: Boolean, override val required: Boolean,
                           default: Option[BigInt], minimum: Option[BigInt], maximum: Option[BigInt]) extends Argument(name, repeated, required)

case class DateArgument(override val name: String, override val repeated: Boolean, override val required: Boolean,
                        default: Option[TemporalAccessor]) extends Argument(name, repeated, required)

case class BooleanArgument(override val name: String, override val repeated: Boolean, override val required: Boolean,
                           default: Option[Boolean]) extends Argument(name, repeated, required)




