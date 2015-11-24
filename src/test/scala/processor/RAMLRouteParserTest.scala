package processor

import java.io.File

import org.apache.commons.io.FileUtils
import org.scalatest.{Assertions, FunSuite}
import play.routes.compiler._

/**
 * @author vadim
 * @since 03.11.15
 */
class RAMLRouteParserTest extends FunSuite with Assertions {
  val parser = new RAMLRouteParser

  val ethalon = List(
    Route(HttpVerb("GET"),
      PathPattern(Seq(StaticPart("users/"), DynamicPart("id", "[^/]+", encode = true))),
      HandlerCall("controllers", "Clients", instantiate = false, "show", Some(Seq(Parameter("id", "Long", None, None))))),

    Route(HttpVerb("PUT"),
      PathPattern(Seq(StaticPart("users/"), DynamicPart("id", "[^/]+", true))),
      HandlerCall("controllers", "Clients", false, "show", Some(Seq(Parameter("id", "Long", None, None))))),

    Route(HttpVerb("GET"),
      PathPattern(Seq(StaticPart("users/"), DynamicPart("id", "[^/]+", true), StaticPart("another/"),
        DynamicPart("id1", "[^/]+", true))),
      HandlerCall("controllers", "Clients", false, "show",
        Some(Seq(Parameter("id", "Long", None, None), Parameter("id1", "Long", None, None)))))

  )

  test("test (implementation) annotations") {
    val result = parser.parseRAML(
      """#%RAML 1.0
        |
        |title: World Music API
        |baseUri: http://example.api.com/{version}
        |version: v1
        |
        |annotationTypes:
        | implementation:
        |  allowMultiple: false
        |  allowedTargets: [ Method ]
        |
        |/users/{id}:
        | get:
        |  description: |
        |   controllers.Clients.show(id: Long)
        |   method get
        |  responses:
        |   200:
        |    application/json:
        |     type: User
        |
        | put:
        |  description: |
        |   controllers.Clients.show(id: Long)
        |  responses:
        |   200:
        |     application/json:
        |       type: User
        |       body:
        |         application/json:
        |           example: |
        |             {"data": "OLOLO1"}
        |           schema: |
        |             {"$schema":"http://json-schema.org/schema", "type":"object", "description":"A test", "properties":{ "data":{"type":"string"} }, "required":[ "data" ]}
        | /another/{id1}:
        |   get:
        |     description: |
        |      controllers.Clients.show(id: Long, id1: Long)
        |""".stripMargin).get

    assert(ethalon == result)
  }

  test("Parser must parse example 2") {
    parser.parse(file("api2.raml")) match {
      case Right(l) => assert(l == ethalon)
      case Left(e) => fail(e.toString())
    }
  }

  test("Parser must parse example 1") {
    val ethalon = List(
      Route(HttpVerb("GET"),
        PathPattern(Seq(StaticPart("rootMethod"))),
        HandlerCall("controllers", "Clients", false, "show3", None)),

      Route(HttpVerb("GET"),
        PathPattern(Seq(StaticPart("testService/"), StaticPart("testMethod"))),
        HandlerCall("controllers", "Clients", false, "show1", None)),

      Route(HttpVerb("GET"),
        PathPattern(Seq(StaticPart("testService/"), StaticPart("testMethodWithParam/"), DynamicPart("param", "[^/]+", true))),
        HandlerCall("controllers", "Clients", false, "show2", Some(Seq(Parameter("id", "Long", None, None))))))

    parser.parse(file("api1.raml")) match {
      case Right(l) =>
        assert(l == ethalon)
      case Left(e) => fail(e.toString())
    }
  }

  private def file(fileName: String): File = {
    val classLoader = getClass.getClassLoader
    new File(classLoader.getResource(fileName).getFile)
  }

  private def fileAsStr(fileName: String): String = FileUtils.readFileToString(file(fileName))
}
