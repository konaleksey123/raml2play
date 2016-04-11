package raml2play.processor

import org.scalatest.{Assertions, FunSuite}
import raml2play.model._

import scala.language.implicitConversions

/**
 * @author vadim
 * @since 01.04.16
 */
class RefParserTest extends FunSuite with Assertions {
  implicit def str2verb(str: String): HttpVerb = HttpVerb(str)

  test("Parse simple static route without params") {
    assert(RefParser.parse("controllers.Clients.test", "get", "/", Seq()).get ==
      Rule("get", PathPattern(Seq()), HandlerCall("controllers", "Clients", false, "test", None)))
  }

  test("Parse complex static route without params") {
    assert(RefParser.parse("test.org.controllers.Clients.test", "get", "/api/test", Seq()).get ==
      Rule("get", PathPattern(Seq(StaticPart("api/test"))), HandlerCall("test.org.controllers", "Clients", false, "test", None)))
  }

  test("Parse complex static route with uri params") {
    assert(RefParser.parse("controllers.Clients.test", "get", "/api/{param}/test", Seq()).get ==
      Rule("get", PathPattern(Seq(StaticPart("api/"), DynamicPart("param", "[^/]+", true), StaticPart("/test"))), HandlerCall("controllers", "Clients", false, "test", None)))
  }

  test("Parse complex static route with uri params 2") {
    assert(RefParser.parse("controllers.Clients.test", "get", "/api/{param}/test/{param2}", Seq()).get ==
      Rule("get", PathPattern(Seq(StaticPart("api/"), DynamicPart("param", "[^/]+", true),
        StaticPart("/test/"), DynamicPart("param2", "[^/]+", true))), HandlerCall("controllers", "Clients", false, "test", None)))
  }

  test("Parse complex static route with uri params 3") {
    assert(RefParser.parse("controllers.Clients.test", "get", "/api/{param}/test/{param2}/", Seq()).get ==
      Rule("get", PathPattern(Seq(StaticPart("api/"), DynamicPart("param", "[^/]+", true),
        StaticPart("/test/"), DynamicPart("param2", "[^/]+", true), StaticPart("/"))), HandlerCall("controllers", "Clients", false, "test", None)))
  }

  test("Parse simple dynamic route without params") {
    assert(RefParser.parse("@controllers.Clients.test", "get", "/", Seq()).get ==
      Rule("get", PathPattern(Seq()), HandlerCall("controllers", "Clients", true, "test", None)))
  }

  test("Parse complex dynamic route without params") {
    assert(RefParser.parse("@test.org.controllers.Clients.test", "get", "/api/{test}", Seq()).get ==
      Rule("get", PathPattern(Seq(StaticPart("api/"), DynamicPart("test", "[^/]+", true))), HandlerCall("test.org.controllers", "Clients", true, "test", None)))
  }

  test("Parse route with params") {
    val params = Seq(
      StringParameter("param1", false, true, None, None, None, None, None),
      IntegerParameter("param2", false, true, None, None, None)
    )

    assert(RefParser.parse("@controllers.Clients.test(param1, param2)", "get", "/",
      IntegerParameter("param3", false, true, None, None, None) +: params).get ==
      Rule("get", PathPattern(Seq()), HandlerCall("controllers", "Clients", true, "test", Some(params))))
  }

  test("Parse route with params (reqex)") {
    val params = Seq(
      StringParameter("param1", false, true, None, Some(".*".r), None, None, None),
      IntegerParameter("param2", false, true, None, None, None)
    )

    assert(RefParser.parse("@controllers.Clients.test(param1, param2)", "get", "/",
      IntegerParameter("param3", false, true, None, None, None) +: params).get ==
      Rule("get", PathPattern(Seq()), HandlerCall("controllers", "Clients", true, "test", Some(params))))
  }

  test("Parse route fail if parameters not defined") {
    val badParams = Seq(
      StringParameter("param1", false, true, None, None, None, None, None),
      IntegerParameter("param1", false, true, None, None, None)
    )

    val expectedParams = Seq(
      StringParameter("param1", false, true, None, None, None, None, None),
      IntegerParameter("param1", false, true, None, None, None)
    )

    val thrown = intercept[RuntimeException] { RefParser.parse(
      "@controllers.Clients.test(param1, param2)", "get", "/",
      IntegerParameter("param3", false, true, None, None, None) +: badParams
    ).get }

    assert(thrown.getMessage === "Argument not defined")
  }
}
