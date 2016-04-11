package raml2play

import java.time.ZonedDateTime

import play.api.mvc.QueryStringBindable
import play.api.mvc.PathBindable

import scala.util.Try

object Bindables {
  implicit def timePathBindable = new PathBindable[ZonedDateTime] {
    def bind(key: String, value: String): Either[String, ZonedDateTime] =
      Try(Right(ZonedDateTime.parse(value))).getOrElse(Left("Bad time format"))

    def unbind(key: String, article: ZonedDateTime): String = article.toString
  }

  implicit def timeQueryBindable = new QueryStringBindable[ZonedDateTime] {
    def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, ZonedDateTime]] =
      params.get(key).map { s =>
        if (s.length > 1) Left("Not a list")
        else if (s.isEmpty) Left("Parameter not present")
        else Try(Right(ZonedDateTime.parse(s.head))).getOrElse(Left("Bad time format"))
      }
    def unbind(key: String, value: ZonedDateTime) = value.toString
  }

  implicit def bigDecimalPathBindable = new PathBindable[BigDecimal] {
    def bind(key: String, value: String): Either[String, BigDecimal] =
      Try(Right(BigDecimal(value))).getOrElse(Left("Bad decimal format"))

    def unbind(key: String, value: BigDecimal): String = value.toString()
  }

  implicit def bigDecimalQueryBindable = new QueryStringBindable[BigDecimal] {
    def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, BigDecimal]] =
      params.get(key).map { s =>
        if (s.length > 1) Left("Not a list")
        else if (s.isEmpty) Left("Parameter not present")
        else Try(Right(BigDecimal(s.head))).getOrElse(Left("Bad decimal format"))
      }
    def unbind(key: String, value: BigDecimal) = value.toString()
  }

  implicit def bigIntPathBindable = new PathBindable[BigInt] {
    def bind(key: String, value: String): Either[String, BigInt] =
      Try(Right(BigInt(value))).getOrElse(Left("Bad integer format"))

    def unbind(key: String, value: BigInt): String = value.toString()
  }

  implicit def bigIntQueryBindable = new QueryStringBindable[BigInt] {
    def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, BigInt]] =
      params.get(key).map { s =>
        if (s.length > 1) Left("Not a list")
        else if (s.isEmpty) Left("Parameter not present")
        else Try(Right(BigInt(s.head))).getOrElse(Left("Bad integer format"))
      }
    def unbind(key: String, value: BigInt) = value.toString()
  }
}
