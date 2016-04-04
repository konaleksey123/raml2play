package processor

import java.io.File

import org.apache.commons.io.FileUtils
import org.scalatest.{Assertions, FunSuite}
import processor.routes_file_parser._

import scala.util.Success

/**
 * @author vadim
 * @since 01.04.16
 */
class RoutesFileParserTest extends FunSuite with Assertions {
  test("Parse RAML with one method") {
    //TODO parser bug - isRequired false by default
    val expectedParams = Seq(
      StringArgument("id", false, false, Some("ololo"), None, None, None, None),
      StringArgument("param1", false, false, None, None, None, None, None),
      NumberArgument("param2", false, false, None, None, None),
      IntegerArgument("param3", false, false, None, None, None),
      DateArgument("param4", false, false, None),
      BooleanArgument("param5", false, false, None),
      StringArgument("param6", false, false, None, None, Some(List("option1","option2","option3")), None, None),
      StringArgument("param7", false, false, None, Some("test*."), None, None, None),
      StringArgument("param8", false, false, None, None, None, Some(1), Some(10)),
      IntegerArgument("param9", false, false, None, Some(1), Some(10)),
      DateArgument("param10", true, false, None),
      DateArgument("param11", false, true, None),
      IntegerArgument("param12", false, false, Some(1), None, None)
    )

    assert(RoutesFileParser.parseFile(file("simpleMethod.raml")) == Success(
      Seq(Rule(true, "get", "/test/{id}", "controllers", "testController", "test", expectedParams))
    ))
  }

  private def file(fileName: String): File = {
    val classLoader = getClass.getClassLoader
    new File(classLoader.getResource(fileName).getFile)
  }
}
