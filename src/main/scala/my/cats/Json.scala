package my.cats

// Define a very simple JSON AST
sealed trait Json

final case class JsObject(get: Map[String, Json]) extends Json
final case class JsString(get: String) extends Json
final case class JsNumber(get: Double) extends Json

// The "serialize to JSON" behavior is encoded in this trait
trait JsonWriter[A] {
  def write(value: A): Json
}

//Interface object
object Json {
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
    w.write(value)
}

object JsonWriterInstances {
  //Type Class Instances
  implicit val stringJsonWriter = new JsonWriter[String] {
    def write(value: String): Json =
      JsString(value)
  }

  implicit val doubleJsonWriter = new JsonWriter[Double] {
    def write(value: Double): Json =
      JsNumber(value)
  }
  implicit val intJsonWriter = new JsonWriter[Int] {
    def write(value: Int): Json =
      JsNumber(value)
  }
  implicit val mapJsonWriter = new JsonWriter[Map[Any, Any]] {
    def write(map: Map[Any, Any]): Json = {
      import JsonSyntax._
      JsObject(
        for {
          s <- map
          (key, value) = s
        } yield {
          val stringKey = key.toString
          val jsonValue = value match {
            case v: Int => v.toJson
            case v: String => v.toJson
            case v: Double => v.toJson
          }
          (stringKey, jsonValue)
        }
      )
    }
  }

}

object JsonSyntax {
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json =
      w.write(value)
  }
}
