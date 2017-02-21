package advcats

// Define a very simple JSON AST
sealed trait Json

final case class JsObject(get: Map[String, Json]) extends Json

final case class JsString(get: String) extends Json

final case class JsNumber(get: Double) extends Json

final case class Person(name: String, email: String)

object Person {
  implicit val personJsonWriter = new JsonWriter[Person] {
    def write(value: Person): Json =
      JsObject(Map(
        "name" -> JsString(value.name),
        "email" -> JsString(value.email)
      ))
  }
}


// The "serialize to JSON" behavior is encoded in this trait
trait JsonWriter[A] {
  def write(value: A): Json
}

object JsonWriterInstances {
  implicit val stringJsonWriter = new JsonWriter[String] {
    def write(value: String): Json =
      JsString(value)
  }

  // etc...
}

// Interface Object
object Json {
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
    w.write(value)
}

// Extension Method
object JsonSyntax {

  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json =
      w.write(value)
  }

}


trait Printer[A] {
  def asString(value: A): String
}

object PrintableInstances {
  implicit val intPrinter = new Printer[Int] {
    override def asString(value: Int) = s"$value"
  }
}

object Printable {
  def format[A](a: A)(implicit x: Printer[A]): String = {
    x.asString(a)
  }
}

object PrintableSyntax {

  implicit class PrintableOps[A](value: A) {
    def format(implicit prin: Printer[A]): String = {
      prin.asString(value)
    }

    def print(implicit prin: Printer[A]): Unit = println(format)
  }

}

final case class Cat(name: String, age: Int, color: String)

object Cat {
  implicit val catPrint = new Printer[Cat] {
    override def asString(value: Cat): String =
      s"${value.name} is a ${value.age} year-old ${value.color} cat."
  }
}