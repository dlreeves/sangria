package sangria.execution

import org.scalatest.{Matchers, WordSpec}
import sangria.parser.QueryParser
import sangria.schema._
import sangria.util.AwaitSupport

import scala.util.Success

import scala.concurrent.ExecutionContext.Implicits.global

class ContextPassingSpec extends WordSpec with Matchers with AwaitSupport {
  trait ColorComponent {
    def color = "green"
  }

  trait NameComponent {
    def name = "foo"
  }

  trait PersonComponent {
    this: NameComponent =>

    def fullName = name + " bar"
  }

  class Cake extends ColorComponent with NameComponent with PersonComponent

  val ColorType = ObjectType("Color", fields[ColorComponent with NameComponent, Unit](
    Field("colorName", StringType)(_.ctx.color),
    Field("name", StringType)(_.ctx.name)))

  val NameType = ObjectType("Name", fields[NameComponent, Unit](
    Field("name", StringType)(_.ctx.name)))

  val PersonType = ObjectType("Person", fields[PersonComponent, Unit](
    Field("fullName", StringType)(_.ctx.fullName),
    Field("name", NameType)(_ => ())))


  val QueryType = ObjectType("Query", fields[Cake, Unit](
    Field("color", ColorType)(_ => ()),
    Field("person", PersonType)(_ => ())
  ))

  val schema = Schema(QueryType)

  "Context" should {
    "should respect inheritance" in {
      val Success(doc) = QueryParser.parse("""
        {
          color {name, colorName}
          person {
            name {name}
            fullName
          }
        }
        """)

      Executor.execute(schema, doc, userContext = new Cake).await should be (Map(
        "data" -> Map(
          "color" -> Map(
            "name" -> "foo",
            "colorName" -> "green"),
          "person" -> Map(
            "name" -> Map("name" -> "foo"),
            "fullName" -> "foo bar"))))
    }
  }

}
