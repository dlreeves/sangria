package sangria.schema

import org.scalatest.{Matchers, WordSpec}

class TypeFieldUniquenessSpec extends WordSpec with Matchers {

  "ObjectType" should {
    "allow unique fields" in {
      ObjectType("Test", fields[Unit, Unit](
        Field("a", StringType)(_ => "foo"),
        Field("b", StringType)(_ => "foo"),
        Field("c", StringType)(_ => "foo")
      ))

      ObjectType("Test", () => fields[Unit, Unit](
        Field("a", StringType)(_ => "foo"),
        Field("b", StringType)(_ => "foo"),
        Field("c", StringType)(_ => "foo")
      )).fields
    }

    "disallow non-unique fields" in {
      an [IllegalArgumentException] should be thrownBy {
        ObjectType("Test", fields[Unit, Unit](
          Field("a", StringType)(_ => "foo"),
          Field("b", StringType)(_ => "foo"),
          Field("a", StringType)(_ => "foo")
        ))
      }

      an [IllegalArgumentException] should be thrownBy {
        ObjectType("Test", () => fields[Unit, Unit](
          Field("a", StringType)(_ => "foo"),
          Field("b", StringType)(_ => "foo"),
          Field("a", StringType)(_ => "foo")
        )).fields
      }
    }
  }

  "InterfaceType" should {
    "allow unique fields" in {
      InterfaceType("Test", fields[Unit, Unit](
        Field("a", StringType)(_ => "foo"),
        Field("b", StringType)(_ => "foo"),
        Field("c", StringType)(_ => "foo")
      ))

      InterfaceType("Test", () => fields[Unit, Unit](
        Field("a", StringType)(_ => "foo"),
        Field("b", StringType)(_ => "foo"),
        Field("c", StringType)(_ => "foo")
      )).fields
    }

    "disallow non-unique fields" in {
      an [IllegalArgumentException] should be thrownBy {
        InterfaceType("Test", fields[Unit, Unit](
          Field("a", StringType)(_ => "foo"),
          Field("b", StringType)(_ => "foo"),
          Field("a", StringType)(_ => "foo")
        ))
      }

      an [IllegalArgumentException] should be thrownBy {
        InterfaceType("Test", () => fields[Unit, Unit](
          Field("a", StringType)(_ => "foo"),
          Field("b", StringType)(_ => "foo"),
          Field("a", StringType)(_ => "foo")
        )).fields
      }
    }
  }

  "InputObjectType" should {
    "allow unique fields" in {
      InputObjectType("Test", List(
        InputField("a", StringType),
        InputField("b", StringType),
        InputField("c", StringType)
      ))

      InputObjectType("Test", () => List(
        InputField("a", StringType),
        InputField("b", StringType),
        InputField("c", StringType)
      )).fields
    }

    "disallow non-unique fields" in {
      an [IllegalArgumentException] should be thrownBy {
        InputObjectType("Test", List(
          InputField("a", StringType),
          InputField("b", StringType),
          InputField("a", StringType)
        ))
      }

      an [IllegalArgumentException] should be thrownBy {
        InputObjectType("Test", () => List(
          InputField("a", StringType),
          InputField("b", StringType),
          InputField("a", StringType)
        )).fields
      }
    }
  }

}
