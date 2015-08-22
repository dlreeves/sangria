package sangria.execution

import org.scalatest.{Matchers, WordSpec}
import sangria.parser.QueryParser
import sangria.schema._
import sangria.util.AwaitSupport

import scala.concurrent.Future
import scala.util.Success

import scala.concurrent.ExecutionContext.Implicits.global

class ActionMapSpec extends WordSpec with Matchers with AwaitSupport {
  case class Color(name: String)

  case class ColorDefer(num: Int) extends Deferred[String]

  class ColorResolver extends DeferredResolver {
    override def resolve(deferred: List[Deferred[Any]]) = deferred map {
      case ColorDefer(num) => Future.successful("[" + (num + 45) + "]")
    }
  }

  val ColorType = ObjectType("Color", fields[Unit, Color](
    Field("name", StringType)(_.value.name)))


  val QueryType = ObjectType("Query", fields[Unit, Unit](
    Field("value", StringType)(_ =>
      Value("red").map("light-" + _)),
    Field("doubleMap", StringType)(_ =>
      Value("red").map("light-" + _).map(_ + "-color")),
    Field("future", StringType)(_ =>
      FutureValue(Future.successful("green")).map("light-" + _)),
    Field("futureDouble", ColorType)(_ =>
      FutureValue(Future.successful("green")).map("light-" + _).map(Color(_))),
    Field("futureTriple", StringType)(_ =>
      FutureValue(Future.successful("green")).map("light-" + _).map(Color(_)).map("super-" + _.name)),
    Field("deferred", StringType)(_ =>
      DeferredValue(ColorDefer(123)).map(x => x + 345)),
    Field("futureDeferred", StringType)(_ =>
      DeferredFutureValue(Future.successful(ColorDefer(34))).map(x => x + 56)),
    Field("futureDeferredDouble", StringType)(_ =>
      DeferredFutureValue(Future.successful(ColorDefer(34))).map(x => x + 576).map("Yay! " + _ + " +++")),
    Field("futureDeferredTriple", StringType)(_ =>
      DeferredFutureValue(Future.successful(ColorDefer(34))).map(x => x + 576).map(Color(_)).map(c => "Yay! " + c.name + " +++")),
    Field("ctxUpdate", ColorType)(ctx =>
      UpdateCtx(DeferredFutureValue(Future.successful(ColorDefer(11)))){v => require(v == "[56]"); ctx.ctx}.map("!" + _ + "?").map(x => x + 576).map(Color(_)).map(c => "(" + c.name + ")").map(Color(_)))
  ))

  val schema = Schema(QueryType)

  "Actions when mapped" should {
    "transform values correctly" in {
      val Success(doc) = QueryParser.parse("""
        {
          value
          doubleMap
          future
          futureDouble {name}
          futureTriple
          deferred
          futureDeferred
          futureDeferredDouble
          futureDeferredTriple
          ctxUpdate {name}
        }
      """)

      Executor.execute(schema, doc, deferredResolver = new ColorResolver).await should be (Map(
        "data" -> Map(
          "value" -> "light-red",
          "doubleMap" -> "light-red-color",
          "future" -> "light-green",
          "futureDouble" -> Map("name" -> "light-green"),
          "futureTriple" -> "super-light-green",
          "deferred" -> "[168]345",
          "futureDeferred" -> "[79]56",
          "futureDeferredDouble" -> "Yay! [79]576 +++",
          "futureDeferredTriple" -> "Yay! [79]576 +++",
          "ctxUpdate" -> Map("name" -> "(![56]?576)")
        )))
    }
  }
}
