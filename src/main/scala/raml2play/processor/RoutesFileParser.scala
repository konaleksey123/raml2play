package raml2play.processor

import java.io.{File, FileInputStream}
import java.time.ZonedDateTime

import org.raml.model._
import org.raml.model.parameter.AbstractParam
import org.raml.parser.visitor.RamlDocumentBuilder
import raml2play.model._

import scala.collection.JavaConversions._
import scala.util.Try

/**
 * @author vadim
 * @since 01.04.16
 */
object RoutesFileParser {
  def parseFile(ramlF: File): Try[List[Rule]] =
    Try(new RamlDocumentBuilder().build(new FileInputStream(ramlF), ramlF.getPath)).map(parse)

  private def parse(raml: Raml): List[Rule] = raml.getResources.flatMap { case (firstPartOfName, r) =>
    traverseToDeep(r, List() /*TODO*/, List())
  }.toList

  private def traverseToDeep(root: Resource, argumentsFromURI: List[Parameter], acc: List[Rule]): List[Rule] = {
    val newArgumentsFromURI = argumentsFromURIParams(root) ++ argumentsFromURI
    val allMethods = methodsFromResource(root, newArgumentsFromURI) ++ acc

    if (root.getResources == null || root.getResources.isEmpty) allMethods
    else {
      root.getResources.map {
        case (partOfName, rawResource) => traverseToDeep(rawResource, newArgumentsFromURI, allMethods)
      }.reduce((a, b) => a ++ b)
    }
  }

  private def argumentsFromURIParams(root: Resource): List[Parameter] =
    if (root.getUriParameters == null) List() else {
      root.getUriParameters.map {
        case (name, param) if param.getType != null => parseArgument(name, param, true)
      }.toList.collect { case Some(param) => param }
    }

  private def parseArgument(name: String, param: AbstractParam, isUriParam: Boolean): Option[Parameter] = {
    val typeO = Option(param.getType)
    val isRepeated = if (isUriParam) false else param.isRepeat
    val isRequired = if (isUriParam) true else param.isRequired
    val defaultO = Option(param.getDefaultValue)

    typeO.collect {
      case ParamType.STRING =>
        val enumO = Option(param.getEnumeration).map(_.toList)
        val patternO = Option(param.getPattern).map(_.r)
        val minLenO = Option(param.getMinLength).map(_.toInt)
        val maxLenO = Option(param.getMaxLength).map(_.toInt)
        StringParameter(name, isRepeated, isRequired, defaultO, patternO, enumO, minLenO, maxLenO)
      case ParamType.BOOLEAN =>
        val default = defaultO.flatMap(v => Try(v.toBoolean.toString).map(Some(_)).getOrElse(None))
        BooleanParameter(name, isRepeated, isRequired, default)
      case ParamType.DATE =>
        //ISO instead RFC2616
        val default = defaultO.flatMap { v =>
          Try(ZonedDateTime.parse(v)).map(d => Some(d.toString)).getOrElse(None)
        }
        DateParameter(name, isRepeated, isRequired, default)
      case ParamType.INTEGER =>
        val default = defaultO.flatMap(v => Try(BigInt(v)).map(d => Some(d.toString())).getOrElse(None))
        val min = Option(param.getMinimum).map(v => BigInt(v.toBigInteger))
        val max = Option(param.getMaximum).map(v => BigInt(v.toBigInteger))
        IntegerParameter(name, isRepeated, isRequired, default, min, max)
      case ParamType.NUMBER =>
        val default = defaultO.flatMap(v => Try(BigDecimal(v)).map(d => Some(d.toString())).getOrElse(None))
        val min = Option(param.getMinimum).map(v => BigDecimal(v.toBigInteger))
        val max = Option(param.getMaximum).map(v => BigDecimal(v.toBigInteger))
        NumberParameter(name, isRepeated, isRequired, default, min, max)
    }
  }

  private def methodsFromResource(r: org.raml.model.Resource, argsFromURI: List[Parameter]): List[Rule] =
    if (r.getActions == null) List()
    else r.getActions.collect {
      case (ActionType.DELETE, action) if action != null && action.getDescription != null =>
        method(action.getDescription, "delete", r.getUri, argumentsFromAction(action) ++ argsFromURI)
      case (ActionType.GET, action) if action != null && action.getDescription != null =>
        method(action.getDescription, "get", r.getUri, argumentsFromAction(action) ++ argsFromURI)
      case (ActionType.HEAD, action) if action != null && action.getDescription != null =>
        method(action.getDescription, "head", r.getUri, argumentsFromAction(action) ++ argsFromURI)
      case (ActionType.OPTIONS, action) if action != null && action.getDescription != null =>
        method(action.getDescription, "options", r.getUri, argumentsFromAction(action) ++ argsFromURI)
      case (ActionType.PATCH, action) if action != null && action.getDescription != null =>
        method(action.getDescription, "patch", r.getUri, argumentsFromAction(action) ++ argsFromURI)
      case (ActionType.POST, action) if action != null && action.getDescription != null =>
        method(action.getDescription, "post", r.getUri, argumentsFromAction(action) ++ argsFromURI)
      case (ActionType.PUT, action) if action != null && action.getDescription != null =>
        method(action.getDescription, "put", r.getUri, argumentsFromAction(action) ++ argsFromURI)
      case (ActionType.TRACE, action) if action != null && action.getDescription != null =>
        method(action.getDescription, "trace", r.getUri, argumentsFromAction(action) ++ argsFromURI)
    }.toList.collect { case Some(m) => m }

  private def argumentsFromAction(a: Action): List[Parameter] =
    a.getQueryParameters.collect { case (name, param) => parseArgument(name, param, false) }.toList.collect { case Some(a) => a }

  private def method(desc: String, verb: String, uri: String, args: List[Parameter]): Option[Rule] =
    RefParser.parse(desc, verb, uri, args).map(Some(_)).getOrElse(None)
}
