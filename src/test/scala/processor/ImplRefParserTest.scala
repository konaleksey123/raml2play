package processor

import org.scalatest.{Assertions, FunSuite}
import processor.routes_file_parser.{Rule, ImplRefParser, IntegerArgument, StringArgument}

/**
 * @author vadim
 * @since 01.04.16
 */
class ImplRefParserTest extends FunSuite with Assertions {
  test("Parse simple static route without params") {
    assert(ImplRefParser.parse("controllers.Clients.test", "get", "/", Seq()).get ==
      Rule(false, "get" ,"/", "controllers", "Clients", "test", Seq()))
  }

  test("Parse complex static route without params") {
    assert(ImplRefParser.parse("test.org.controllers.Clients.test", "get", "/api", Seq()).get ==
      Rule(false, "get" ,"/api", "test.org.controllers", "Clients", "test", Seq()))
  }

  test("Parse simple dynamic route without params") {
    assert(ImplRefParser.parse("@controllers.Clients.test", "get", "/", Seq()).get ==
      Rule(true, "get" ,"/", "controllers", "Clients", "test", Seq()))
  }

  test("Parse complex dynamic route without params") {
    assert(ImplRefParser.parse("@test.org.controllers.Clients.test", "get", "/api", Seq()).get ==
      Rule(true, "get" ,"/api", "test.org.controllers", "Clients", "test", Seq()))
  }

  test("Parse route with params") {
    val params = Seq(
      StringArgument("param1", false, true, None, None, None, None, None),
      IntegerArgument("param2", false, true, None, None, None)
    )

    assert(ImplRefParser.parse("@controllers.Clients.test(param1, param2)", "get", "/",
      IntegerArgument("param3", false, true, None, None, None) +: params).get ==
      Rule(true, "get" ,"/", "controllers", "Clients", "test", params))
  }

  test("Parse route fail if parameters not defined") {
    val badParams = Seq(
      StringArgument("param1", false, true, None, None, None, None, None),
      IntegerArgument("param1", false, true, None, None, None)
    )

    val expectedParams = Seq(
      StringArgument("param1", false, true, None, None, None, None, None),
      IntegerArgument("param1", false, true, None, None, None)
    )

    val thrown = intercept[RuntimeException] { ImplRefParser.parse(
      "@controllers.Clients.test(param1, param2)", "get", "/",
      IntegerArgument("param3", false, true, None, None, None) +: badParams
    ).get }

    assert(thrown.getMessage === "Argument not defined")
  }
}
