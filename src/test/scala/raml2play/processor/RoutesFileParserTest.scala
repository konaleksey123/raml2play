package raml2play.processor

import java.io.File

import org.scalatest.{Assertions, FunSuite}
import raml2play.model._

import scala.util.Success

/**
 * @author vadim
 * @since 01.04.16
 */
class RoutesFileParserTest extends FunSuite with Assertions {
  test("Parse RAML with one method") {
    //TODO parser bug - isRequired false by default
    val expectedParams = Seq(
      StringParameter("id", false, true, Some("ololo"), Some("ololo*.".r), None, None, None),
      StringParameter("param1", false, false, None, None, None, None, None),
      NumberParameter("param2", false, false, None, None, None),
      IntegerParameter("param3", false, false, None, None, None),
      DateParameter("param4", false, false, None),
      BooleanParameter("param5", false, false, None),
      StringParameter("param6", false, false, None, None, Some(List("option1","option2","option3")), None, None),
      StringParameter("param7", false, false, None, Some("test*.".r), None, None, None),
      StringParameter("param8", false, false, None, None, None, Some(1), Some(10)),
      IntegerParameter("param9", false, false, None, Some(1), Some(10)),
      DateParameter("param10", true, false, None),
      DateParameter("param11", false, true, None),
      IntegerParameter("param12", false, false, Some("1"), None, None)
    )

    assert(RoutesFileParser.parseFile(file("queryParams.raml")) == Success(
      Seq(Rule(HttpVerb("get"), PathPattern(Seq(StaticPart("test/"),
        DynamicPart("id", "ololo*.", true))), HandlerCall("controllers", "testController", true, "test", Some(expectedParams))))
    ))
  }

  private def file(fileName: String): File = {
    val classLoader = getClass.getClassLoader
    new File(classLoader.getResource(fileName).getFile)
  }
}
