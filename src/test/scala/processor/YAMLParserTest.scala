package processor

import java.io.File

import org.apache.commons.io.FileUtils
import org.scalatest.{Assertions, FunSuite}

/**
 * @author vadim
 * @since 02.11.15
 */
class YAMLParserTest extends FunSuite with Assertions {
  val grammar = new YAMLParser

  test("Empty inline map") {
    assert(Map() == grammar.parse("""{}""").get)
  }

  test("Empty inline list") {
    assert(List() == grammar.parse("""[]""").get)
  }

  test("With empty inline map") {
    assert(
      List("foo", Map(), "bar") ==
      grammar.parse("""|- foo
              		         |- {}
              			     |- bar""".stripMargin).get)
  }

  test("empty inline list and map") {
    assert(
      Map("foo_bar" -> "true", "snafu" -> List(), "empty" -> Map()) ==
      grammar.parse("""|foo_bar: true
              				 |snafu: []
              				 |empty: {}""".stripMargin).get)
  }

  test("simple map") {
    assert(Map("key"->"value") == grammar.parse("""key: value""").get)
    assert(Map("key1"->"value1", "key2"->"value2") == grammar.parse(
      """|
        				   |key1: value1
        				   |key2: value2
        				   |""".stripMargin).get)
    assert(Map("key1"->"value 1", "key2"->"value 2") == grammar.parse(
      """|
        				   |key1: value 1
        				   |key2: value 2
        				   |""".stripMargin).get)
  }

  test("simple list") {
    assert(List("item1") == grammar.parse(
      """|
        				   |- item1
        				   |""".stripMargin).get)
    assert(List("item1", "item2") == grammar.parse(
      """|
        				   |- item1
        				   |- item2
        				   |""".stripMargin).get)
  }

  test("list of list") {
    assert(List(
      List("item11", "item12"),
      List("item21", "item22")) ==
      grammar.parse(
        """|
          						|-
          						| - item11
          						| - item12
          						|-
          						| - item21
          						| - item22
          						|""".stripMargin).get)
  }

  test("map of map") {
    assert(Map("JFrame" -> Map("name" -> "myFrame", "title" -> "My App Frame")) ==
      grammar.parse(
        """|
          						|JFrame:
          | name: myFrame
          | title: My App Frame
          						|""".stripMargin).get)

    assert(Map("JFrame" -> Map("name" -> "myFrame", "title" -> "My App Frame")) ==
      grammar.parse(
        """|
          						|JFrame:
                      |  name: myFrame
                      |  title: My App Frame
          						|""".stripMargin).get)

    assert(Map("Mark McGwire" -> Map("hr" -> "65", "avg" -> "0.278"), "Sammy Sosa" -> Map("hr" -> "63", "avg" -> "0.288"))
      == grammar.parse("""
                             |Mark McGwire: {hr: 65, avg: 0.278}
                             |Sammy Sosa: {
                             |     hr: 63,
                             |   avg: 0.288
                             |}
                           """.stripMargin).get)
  }

  test("list of map") {
    assert(List(
      Map("name"-> "John Smith", "age"->"33"),
      Map("name"-> "Mary Smith", "age"->"27")) ==
      grammar.parse(
        """|
          						|- name: John Smith
          						|  age: 33
          						|- name: Mary Smith
          						|  age: 27
          						|""".stripMargin).get)
  }

  test("map of list") {
    assert(Map("men" -> List("John Smith", "Bill Jones"), "women" -> List("Mary Smith", "Susan Williams")) ==
      grammar.parse(
        """|
          					   |men:
          					   | - John Smith
          					   | - Bill Jones
          					   |women:
          					   | - Mary Smith
          					   | - Susan Williams
          					   |""".stripMargin).get)

    assert(Map("Sammy Sosa" -> List("63", "0.288"))
      == grammar.parse("""
      |Sammy Sosa: [
      |     63,
      |   0.288
      |]
    """.stripMargin).get)
  }

  test("inline map") {
    assert(Map("key"->"value") == grammar.parse("""key: value""").get)
    assert(Map("key1"->"value1", "key2"->"value2") == grammar.parse(
      """{ key1: value1, key2: value2 }""").get)
  }

  test("map of map of map") {
    assert(Map("JFrame" -> Map("content" -> Map("button" -> "press"))) ==
      grammar.parse(
        """|
                    |JFrame:
                    | content:
                    |  button: press
                    |""".stripMargin).get)

    assert(Map("JFrame" -> Map("content" -> Map("button" -> "press")), "Ololo" -> Map("content" -> Map("button" -> "press"))) ==
      grammar.parse(
        """|
          |JFrame:
          |  content:
          |    button: press
          |Ololo:
          |  content:
          |    button: press
          |""".stripMargin).get)
  }

  test("inline list") {
    assert(List("item1", "item2") ==
      grammar.parse("""[ item1, item2 ]""").get)
  }

  test("map of inline list") {
    assert(Map("men" -> List("John Smith", "Bill Jones"), "women" -> List("Mary Smith", "Susan Williams")) ==
      grammar.parse(
        """|
          					   |men: [ John Smith, Bill Jones ]
          					   |women: [ Mary Smith, Susan Williams ]
          					   |""".stripMargin).get)
  }

  test("more complicated") {
    assert(
      Map("address" ->
        Map("first_name" -> "Brian",
          "last_name" -> "Reece",
          "email" -> "brian@majordomo.com",
          "company" ->
            Map("name"->"Five Apart, Ltd.",
              "street_address"->"8458 5th Street, San Francisco, CA 94107"))) == grammar.parse(
        """|address:
          	  | first_name: Brian
          	  | last_name: Reece
          	  | email: brian@majordomo.com
          	  | company:
          	  |  name: Five Apart, Ltd.
          	  |  street_address: 8458 5th Street, San Francisco, CA 94107
        """.stripMargin).get)
  }

  test("comments") {
    assert(Map("hr" -> List("Mark McGwire", "Sammy Sosa"), "rbi" -> List("Sammy Sosa", "Ken Griffey")) ==
      grammar.parse("""hr: # 1998 hr ranking
      | - Mark McGwire
      | - Sammy Sosa
      |rbi:
      | # 1998 rbi ranking
      | - Sammy Sosa
      | - Ken Griffey
    """.stripMargin).get)

    assert(grammar.parse("""hr:
      | - Mark McGwire
      | # Following node labeled SS
      | - &SS Sammy Sosa
      |rbi:
      | - *SS # Subsequent occurrence
      | - Ken Griffey""".stripMargin).get == Map("hr" -> List("Mark McGwire", "&SS Sammy Sosa"), "rbi" -> List("*SS", "Ken Griffey")))
  }

  test("block skalars") {
    assert(grammar.parse("""
      |name: Mark McGwire
      |accomplishment: >
      | Mark set a major league
      | home run record in 1998.
      |stats: |
      | 65 Home Runs
      | 0.278 Batting Average
      |other: OK
      | """.stripMargin).get ==
      Map("name" -> "Mark McGwire",
        "accomplishment" -> "Mark set a major league home run record in 1998.",
        "stats" -> "\n65 Home Runs\n0.278 Batting Average", "other" -> "OK"))
  }

  test("Parser must parse file 1") {
    val ethalon = Map("title" -> "Test API",
      "baseUri" -> "http://127.0.0.1:9000/",
      "version" -> "v1",
      "/rootMethod" -> Map(
        "get" -> Map(
          "description" -> "\ncontrollers.Clients.show3")),
      "/testService" -> Map(
        "/testMethod" -> Map(
          "description" -> "OLOLO OLOLO OLOLO",
          "get" -> Map(
            "responses" -> Map(
              "200" -> Map(
                "body" -> Map(
                  "application/json" -> Map("example" -> """|
                                          |{
                                          |"data": "OLOLO1"
                                          |}""".stripMargin)))),
            "description" -> "\ncontrollers.Clients.show1\nRetrieve a test JSON")),
        "/testMethodWithParam" -> Map(
          "description" -> "With Param!!!",
          "/{param}" -> Map(
            "uriParameters" -> Map(
              "param" -> Map(
                "displayName" -> "User ID",
                "description" -> "With Param!!!")),
            "get" -> Map(
              "description" -> "\ncontrollers.Clients.show2(id: Long)")))))
    val f1 = grammar.parse(fileAsStr("api1.raml")).get
    assert(f1 == ethalon)
  }

  private def file(fileName: String): File = {
    val classLoader = getClass.getClassLoader
    new File(classLoader.getResource(fileName).getFile)
  }

  private def fileAsStr(fileName: String): String = FileUtils.readFileToString(file(fileName))
}

