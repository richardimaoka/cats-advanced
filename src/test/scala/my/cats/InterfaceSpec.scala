package my.cats

import org.scalatest._

class InterfaceSpec extends FlatSpec with Matchers {

  def logit(value: => Any): Unit = info(value.toString)

  "Interface object" should "work for int" in {
    import my.cats.JsonWriterInstances.intJsonWriter

    val actual = Json.toJson(1)
    val expected = JsNumber(1.0)
    logit(actual)
    logit(expected)
    //JsNumber1.0)
    actual should equal(expected)
  }

  "Interface object" should "work for double" in {
    import my.cats.JsonWriterInstances.doubleJsonWriter

    val actual = Json.toJson(1.0)
    val expected = JsNumber(1.0)
    logit(actual)
    logit(expected)
    //JsNumber1(1.0)
    actual should equal(expected)
  }

  "Interface object" should "work for string" in {
    import my.cats.JsonWriterInstances.stringJsonWriter

    val actual = Json.toJson("spapapa")
    val expected = JsString("spapapa")
    logit(actual)
    logit(expected)
    //JsString(spapapa)
    actual should equal(expected)
  }

  "Interface object" should "work for person" in {
    import my.cats.Person._

    val actual = Json.toJson(Person("Dave", "dave@example.com"))
    val expected = JsObject(Map(
      "name" -> JsString("Dave"),
      "email" -> JsString("dave@example.com")
    ))
    logit(actual)
    logit(expected)
    //JsObject(Map(name -> JsString(Dave), email -> JsString(dave@example.com)))
    actual should equal(expected)
  }

  "Interface object" should "work for map" in {
    import my.cats.JsonWriterInstances._

    val map: Map[Any,Any] = Map(10 -> "asfsdfs", 20 -> "sadfsfsf", "sdfsdfsdfsd" -> "|sdfasfdsfsaf!")

    val actual = Json.toJson(map)
    val expected = JsObject(Map(
      "10" -> JsString("asfsdfs"),
      "20" -> JsString("sadfsfsf"),
      "sdfsdfsdfsd" -> JsString("|sdfasfdsfsaf!")
    ))
    logit(actual)
    logit(expected)
    actual should equal(expected)
  }

  /**
   * Nested map needs more tricks, since implicit `mapJsonWriter`
   * doesn't handle the map inside map in its pattern match
   */
  //  "Interface object" should "work for a nested map" in {
//    import my.cats.JsonWriterInstances._
//
//    val map: Map[Any,Any] = Map(
//      10 -> "asfsdfs",
//      20 -> "sadfsfsf",
//      "sdfsdfsdfsd" -> Map(
//        "|sdfasfdsfsaf!" -> 4000,
//        "s*af*sd*afsd*fsd!" -> -4000
//      )
//    )
//
//    val actual = Json.toJson(map)
//    val expected = JsObject(Map(
//      "10" -> JsString("asfsdfs"),
//      "20" -> JsString("sadfsfsf"),
//      "sdfsdfsdfsd" -> JsObject(Map(
//        "|sdfasfdsfsaf!" -> JsNumber(4000),
//        "s*af*sd*afsd*fsd!" -> JsNumber(-4000)
//      ))
//    ))
//    logit(actual)
//    logit(expected)
////    actual should equal(expected)
//  }

  "Interface syntax" should "work for int" in {
    import my.cats.JsonWriterInstances.intJsonWriter
    import JsonSyntax._

    val actual = 1.toJson
    val expected = JsNumber(1.0)
    logit(actual)
    logit(expected)
    //JsNumber1.0)
    actual should equal(expected)
  }

  "Interface syntax" should "work for double" in {
    import my.cats.JsonWriterInstances.doubleJsonWriter
    import JsonSyntax._

    val actual = 1.0.toJson
    val expected = JsNumber(1.0)
    logit(actual)
    logit(expected)
    //JsNumber1(1.0)
    actual should equal(expected)
  }

  "Interface syntax" should "work for string" in {
    import my.cats.JsonWriterInstances.stringJsonWriter
    import JsonSyntax._

    val actual = Json.toJson("spapapa")
    val expected = JsString("spapapa")
    logit(actual)
    logit(expected)
    //JsString(spapapa)
    actual should equal(expected)
  }

  "Interface syntax" should "work for person" in {
    import my.cats.Person._
    import JsonSyntax._

    val actual = Person("Dave", "dave@example.com").toJson
    val expected = JsObject(Map(
      "name" -> JsString("Dave"),
      "email" -> JsString("dave@example.com")
    ))
    logit(actual)
    logit(expected)
    //JsObject(Map(name -> JsString(Dave), email -> JsString(dave@example.com)))
    actual should equal(expected)
  }
}
