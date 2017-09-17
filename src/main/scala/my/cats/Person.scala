package my.cats

final case class Person(name: String, email: String)

object Person {
  implicit val personJsonWriter = new JsonWriter[Person] {
    def write(value: Person): Json =
      JsObject(Map(
        "name" -> JsString(value.name),
        "email" -> JsString(value.email)
      ))
  }

  implicit val personPrintable = new Printable[Person] {
    def format(value: Person): String = s"name = ${value.name}, email = ${value.email}"
  }
}
