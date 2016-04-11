/*
 * Copyright (C) 2009-2016 Lightbend Inc. <https://www.lightbend.com>
 */
package raml2play

import java.time.ZonedDateTime

import raml2play.model._

import scala.collection.immutable.ListMap
import scala.util.matching.Regex

/**
 * Helper methods used in the templates
 */
object TemplateHelpers {
  /**
   * Generate a base identifier for the given route
   */
  def baseIdentifier(route: Rule, index: Int): String = 
    route.call.packageName.replace(".", "_") + "_" + route.call.controller.replace(".", "_") + "_" + route.call.method + index

  /**
   * Generate a route object identifier for the given route
   */
  def routeIdentifier(route: Rule, index: Int): String = baseIdentifier(route, index) + "_route"

  /**
   * Generate a invoker object identifier for the given route
   */
  def invokerIdentifier(route: Rule, index: Int): String = baseIdentifier(route, index) + "_invoker"

  def concatSep[T](seq: Seq[T], sep: String)(f: T => ScalaContent): Any = {
    if (seq.isEmpty) {
      Nil
    } else {
      Seq(f(seq.head), seq.tail.map { t =>
        Seq(sep, f(t))
      })
    }
  }

  /**
   * Generate a controller method call for the given route
   */
  def controllerMethodCall(r: Rule, paramFormat: Parameter => String): String = {
    val methodPart = if (r.call.instantiate) {
      s"$Injector.instanceOf(classOf[${r.call.packageName}.${r.call.controller}]).${r.call.method}"
    } else {
      s"${r.call.packageName}.${r.call.controller}.${r.call.method}"
    }
    val paramPart = r.call.parameters.map { params =>
      params.map(paramFormat).mkString(", ")
    }.map("(" + _ + ")").getOrElse("")
    methodPart + paramPart
  }

  /**
   * Generate a controller method call for the given injected route
   */
  def injectedControllerMethodCall(r: Rule, ident: String, paramFormat: Parameter => String): String = {
    val methodPart = if (r.call.instantiate) {
      s"$ident.get.${r.call.method}"
    } else {
      s"$ident.${r.call.method}"
    }
    val paramPart = r.call.parameters.map { params =>
      params.map(paramFormat).mkString(", ")
    }.map("(" + _ + ")").getOrElse("")
    methodPart + paramPart
  }

  def paramNameOnQueryString(paramName: String) = {
    if (paramName.matches("^`[^`]+`$"))
      paramName.substring(1, paramName.length - 1)
    else
      paramName
  }

  /**
   * A route binding
   */
  def routeBinding(route: Rule): String = {
    route.call.parameters.filterNot(_.isEmpty).map { params =>
      val ps = params.map { p =>
        val paramName: String = paramNameOnQueryString(p.name)

        """params.""" + (if (route.path.has(paramName)) "fromPath" else "fromQuery") + """[""" +
          p.paramType + """]("""" + paramName + """", """ + p.default.map("Some(" + _ + ")").getOrElse("None") + """)"""
      }
      if (ps.size < 22) ps.mkString(", ") else ps
    }.map("(" + _ + ")").getOrElse("")
  }

  /**
   * Extract the local names out from the route, as tuple. See PR#4244
   */
  def tupleNames(route: Rule) = route.call.parameters.filterNot(_.isEmpty).map { params =>
    params.map(x => safeKeyword(x.name)).mkString(", ")
  }.map("(" + _ + ") =>").getOrElse("")

  /**
   * The code to statically get the Play injector
   */
  val Injector = "play.api.Play.routesCompilerMaybeApplication.map(_.injector).getOrElse(play.api.inject.NewInstanceInjector)"

  val scalaReservedWords = List(
    "abstract", "case", "catch", "class",
    "def", "do", "else", "extends",
    "false", "final", "finally", "for",
    "forSome", "if", "implicit", "import",
    "lazy", "macro", "match", "new",
    "null", "object", "override", "package",
    "private", "protected", "return", "sealed",
    "super", "then", "this", "throw",
    "trait", "try", "true", "type",
    "val", "var", "while", "with",
    "yield",
    // Not scala keywords, but are used in the router
    "queryString"
  )

  /**
   * Ensure that the given keyword doesn't clash with any of the keywords that Play is using, including Scala keywords.
   */
  def safeKeyword(keyword: String) =
    scalaReservedWords.collectFirst {
      case reserved if reserved == keyword => s"_pf_escape_$reserved"
    }.getOrElse(keyword)

  /**
   * Generate the ref router call
   */
  def refCall(route: Rule, useInjector: Rule => Boolean): String = {
    val controllerRef = s"${route.call.packageName}.${route.call.controller}"
    val methodCall = s"${route.call.method}(${
      route.call.parameters.getOrElse(Nil).map(x => safeKeyword(x.name)).mkString(", ")
    })"
    if (useInjector(route)) {
      s"$Injector.instanceOf(classOf[$controllerRef]).$methodCall"
    } else {
      s"$controllerRef.$methodCall"
    }
  }

  /**
   * Encode the given String constant as a triple quoted String.
   *
   * This will split the String at any $ characters, and use concatenation to concatenate a single $ String followed
   * be the remainder, this is to avoid "possible missing interpolator" false positive warnings.
   *
   * That is to say:
   *
   * {{{
   * /foo/$id<[^/]+>
   * }}}
   *
   * Will be encoded as:
   *
   * {{{
   *   """/foo/""" + "$" + """id<[^/]+>"""
   * }}}
   */
  def encodeStringConstant(constant: String) = {
    constant.split('$').mkString(tq, s"""$tq + "$$" + $tq""", tq)
  }

  val ob = "{"
  val cb = "}"
  val tq = "\"\"\""

  def paramChecker(param: Parameter): String = {
    def checkStrLenAndPattern(str: StringParameter): String = {
      def minLenChecker(min: Option[Int]): String = min.fold("") { m =>
        s"""if (${param.name}.length < $m) badRequest("Argument to short") else"""
      }
      def maxLenChecker(max: Option[Int]): String = max.fold("") { m =>
        s"""if (${param.name}.length > $m) badRequest("Argument to big") else """
      }
      def patternChecker(pattern: Option[Regex]): String = pattern.fold("") { p =>
        val cond = "\"" + p.toString() + "\"" + s".r.findFirstIn(${param.name}).isDefined"
        s"""if (!$cond) badRequest("Regex condition is violated") else """
      }
      def enumChecker(enum: Option[List[String]]): String = enum.fold("") { l =>
        val cond = "List(" + l.map("\"" + _ + "\"").reduce((s1, s2) => s1 + "," + s2) + ")" + s".contains(${param.name})"
        s"""if (!$cond) badRequest("Enum condition is violated") else """
      }

      minLenChecker(str.minLength) + " " + maxLenChecker(str.maxLength) + " " + enumChecker(str.enum) + " " +
        patternChecker(str.pattern)
    }

    def numberChecker(number: NumberParameter): String = {
      def minChecker(min: Option[BigDecimal]): String = min.fold("") { m =>
        s"""if (${param.name} < $m) badRequest("Argument to small") else """
      }
      def maxChecker(max: Option[BigDecimal]): String = max.fold("") { m =>
        s"""if (${param.name} > $m) badRequest("Argument to large") else """
      }

      minChecker(number.minimum) + "\n" + maxChecker(number.maximum)
    }

    def integerChecker(integer: IntegerParameter): String = {
      def minChecker(min: Option[BigInt]): String = min.fold("") { m =>
        s"""if (${param.name} < $m) badRequest("Argument to small") else """
      }
      def maxChecker(max: Option[BigInt]): String = max.fold("") { m =>
        s"""if (${param.name} > $m) badRequest("Argument to large") else """
      }

      minChecker(integer.minimum) + " " + maxChecker(integer.maximum)
    }

    param match {
      case p: StringParameter => checkStrLenAndPattern(p)
      case p: IntegerParameter => integerChecker(p)
      case p: NumberParameter => numberChecker(p)
      case _ => ""
    }
  }

  def paramsChecker(route: Rule): String = route.call.parameters.map(_.map(paramChecker).mkString("\n")).getOrElse("")
}

