package advcats

import org.scalatest.{Matchers, WordSpec}

class TypeClassesTest extends WordSpec with Matchers {

  "JsonWriter" must {

    "Convert a Person to Json" when {
      val dave = Person("Dave", "dave@example.com")

      "An Interface Object is used" should {
        val json: Json = Json.toJson(dave)

        "Create a JsObject" in {
          json.isInstanceOf[JsObject] should be(true)
        }

        val map = json.asInstanceOf[JsObject].get
        "Map the name and email fields to keys" in {
          map.keys should contain("name")
          map.keys should contain("email")
        }
      }

      "An extention method is used" should {
        import advcats.JsonSyntax._
        val json = dave.toJson

        "Create a JsObject" in {
          json.isInstanceOf[JsObject] should be(true)
        }

        val map = json.asInstanceOf[JsObject].get
        "Map the name and email fields to keys" in {
          map.keys should contain("name")
          map.keys should contain("email")
        }
      }
    }

    "Not compile when attempting to converting a non member of the Json type class" in {
        // Uncomment to see compilation failures
        // val json: Json = Json.toJson(4)
    }
  }

  "Printable" should {
    val int = 7

    "Print an integer using an Interface Object" in {
      import advcats.PrintableInstances._
      val result = Printable.format(int)
      result should be("7")
    }

    "Print an integer using an Extension Methods" in {
      import advcats.PrintableInstances._
      import advcats.PrintableSyntax._
      val result = int.format
      result should be("7")
      int.print
    }

  }

  "A Cat" should {

    val cat = Cat("NAME", 10, "COLOR")
    "Format to a string" in {
      import advcats.PrintableSyntax._
      cat.format should be("NAME is a 10 year-old COLOR cat.")
    }
  }
}
