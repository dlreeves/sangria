package sangria.execution

import org.scalatest.{Matchers, WordSpec}
import sangria.parser.QueryParser
import sangria.schema._
import sangria.util.AwaitSupport
import sangria.validation.QueryValidator

import scala.concurrent.Future
import scala.util.Success
import scala.concurrent.ExecutionContext.Implicits.global

class ExecutorSpec extends WordSpec with Matchers with AwaitSupport {
  class TestSubject {
    def a: Option[String] = Some("Apple")
    def b: Option[String] = Some("Banana")
    def c: Option[String] = Some("Cookie")
    def d: Option[String] = Some("Donut")
    def e: Option[String] = Some("Egg")
    val f: Option[String] = Some("Fish")
    def deep: Option[DeepTestSubject] = Some(new DeepTestSubject)
    def deepColor(c: String): DeepTestSubject = new DeepTestSubject(c)
    def pic(size: Option[Int]) = "Pic of size: " + (size getOrElse 50)
    def future: Future[Option[TestSubject]] = Future.successful(Some(new TestSubject))
  }

  class DeepTestSubject(val color: String = "none") {
    def a: Option[String] = Some("Already Been Done")
    def b: Option[String] = Some("Boring")
    def c: List[Option[String]] = Some("Contrived") :: None :: Some("Confusing") :: Nil
    def deeper: List[Option[TestSubject]] = Some(new TestSubject) :: null :: Some(new TestSubject) :: Nil
  }

  case class Ctx(color: String = "green")

  case class LightColor(subj: TestSubject, color: String) extends Deferred[DeepTestSubject]
  case class FailColor(subj: TestSubject, color: String) extends Deferred[DeepTestSubject]

  class LightColorResolver extends DeferredResolver {
    def resolve(deferred: List[Deferred[Any]]) = deferred map {
      case LightColor(v, c) => Future.successful(v.deepColor("light" + c))
      case FailColor(v, c) => Future.failed(new IllegalStateException("error in resolver"))
    }
  }

  val DeepDataType = ObjectType("DeepDataType", () => fields[Ctx, DeepTestSubject](
    Field("a", OptionType(StringType))(_.value.a),
    Field("b", OptionType(StringType))(_.value.b),
    Field("c", OptionType(ListType(OptionType(StringType))))(_.value.c),
    Field("color", StringType)(_.value.color),
    Field("ctxColor", OptionType(StringType))(_.ctx.color),
    Field("deeper", OptionType(ListType(OptionType(DataType))))(_.value.deeper)
  ))

  val DataType: ObjectType[Ctx, TestSubject] = ObjectType("DataType", () => fields[Ctx, TestSubject](
    Field("a", OptionType(StringType))(_.value.a),
    Field("b", OptionType(StringType))(_.value.b),
    Field("c", OptionType(StringType))(_.value.c),
    Field("d", OptionType(StringType))(_.value.d),
    Field("e", OptionType(StringType))(_.value.e),
    Field("f", OptionType(StringType))(_.value.f),
    Field("ctxUpdating", DeepDataType)(
      ctx => UpdateCtx(ctx.value.deepColor("blue"))(v => ctx.ctx.copy(color = v.color))),
    Field("ctxUpdatingFut", DeepDataType)(
      ctx => UpdateCtx(Future.successful(ctx.value.deepColor("orange")))(v => ctx.ctx.copy(color = v.color))),
    Field("ctxUpdatingDef", DeepDataType)(
      ctx => UpdateCtx(LightColor(ctx.value, "magenta"))(v => ctx.ctx.copy(color = v.color))),
    Field("ctxUpdatingDefFut", DeepDataType)(
      ctx => UpdateCtx(DeferredFutureValue(Future.successful(LightColor(ctx.value, "red"))))(v => ctx.ctx.copy(color = v.color))),
    Field("def", DeepDataType)(ctx => LightColor(ctx.value, "magenta")),
    Field("defFut", DeepDataType)(ctx => DeferredFutureValue(Future.successful(LightColor(ctx.value, "red")))),
    Field("defFail", OptionType(DeepDataType))(ctx => FailColor(ctx.value, "magenta")),
    Field("defFutFail", OptionType(DeepDataType))(ctx => DeferredFutureValue(Future.successful(FailColor(ctx.value, "red")))),
    Field("pic", OptionType(StringType), arguments = Argument("size", OptionInputType(IntType)) :: Nil)(
      ctx => ctx.value.pic(ctx.args.arg[Option[Int]]("size"))),
    Field("deep", OptionType(DeepDataType))(_.value.deep),
    Field("future", OptionType(DataType))(_.value.future)
  ))

  val ParallelFragmentType: ObjectType[Unit, Unit] = ObjectType("Type", () => fields[Unit, Unit](
    Field("a", OptionType(StringType))(_ => "Apple"),
    Field("b", OptionType(StringType))(_ => "Banana"),
    Field("c", OptionType(StringType))(_ => "Cherry"),
    Field("deep", OptionType(ParallelFragmentType))(_ => ())
  ))

  "Execute: Handles basic execution tasks" should {
    "execute arbitrary code" in {
      val Success(doc) = QueryParser.parse("""
        query Example($size: Int) {
          a,
          b,
          x: c
          ...c
          f
          ...on DataType {
            pic(size: $size)
            future {
              a
            }
          }
          deep {
            a
            b
            c
            deeper {
              a
              b
            }
          }
        }

        fragment c on DataType {
          d
          e
        }
      """)

      val expected = Map(
        "data" -> Map(
          "a" -> "Apple",
          "b" -> "Banana",
          "x" -> "Cookie",
          "d" -> "Donut",
          "e" -> "Egg",
          "f" -> "Fish",
          "pic" -> "Pic of size: 100",
          "future" -> Map("a" -> "Apple"),
          "deep" -> Map(
            "a" -> "Already Been Done",
            "b" -> "Boring",
            "c" -> List("Contrived", null, "Confusing"),
            "deeper" -> List(
              Map("a" -> "Apple", "b" -> "Banana"),
              null,
              Map("a" -> "Apple", "b" -> "Banana")
            )
          )
        )
      )

      val schema = Schema(DataType)

      Executor(schema, new TestSubject, Ctx()).execute(doc, arguments = Some(Map("size" -> 100))).await should be (expected)
    }


    "merge parallel fragments" in {
      val schema = Schema(ParallelFragmentType)

      val Success(doc) = QueryParser.parse("""
        { a, ...FragOne, ...FragTwo }

        fragment FragOne on Type {
          b
          deep { b, deeper: deep { b } }
        }

        fragment FragTwo on Type {
          c
          deep { c, deeper: deep { c } }
        }
      """)

      val expected = Map(
        "data" -> Map(
          "a" -> "Apple",
          "b" -> "Banana",
          "c" -> "Cherry",
            "deep" -> Map(
            "b" -> "Banana",
            "c" -> "Cherry",
              "deeper" -> Map(
              "b" -> "Banana",
              "c" -> "Cherry")))
      )

      Executor(schema).execute(doc).await should be (expected)
    }

    "threads context correctly" in {
      case class Thing(a: Option[String])

      var resolvedCtx: Option[String] = None

      val schema = Schema(ObjectType("Type", fields[Unit, Thing](
        Field("a", OptionType(StringType))(ctx => {resolvedCtx = ctx.value.a; ctx.value.a}))))

      val Success(doc) = QueryParser.parse("query Example { a }")
      Executor(schema, Thing(Some("thing"))).execute(doc).await should be (Map("data" -> Map("a" -> "thing")))
      resolvedCtx should be (Some("thing"))
    }

    "correctly threads arguments" in {
      var resolvedArgs: Args = Args.empty

      val schema = Schema(ObjectType("Type", fields[Unit, Unit](
        Field("b", OptionType(StringType),
            arguments =
              Argument("numArg", OptionInputType(IntType)) ::
              Argument("stringArg", OptionInputType(StringType)) :: Nil) { ctx =>
          resolvedArgs = ctx.args
          None
        })))

      val Success(doc) = QueryParser.parse("""
        query Example {
          b(numArg: 123, stringArg: "foo")
        }
      """)

      Executor(schema).execute(doc).await
      resolvedArgs should be (Args(Map("numArg" -> 123, "stringArg" -> "foo")))
    }

    "null out error subtrees" in {
      class Data {
        def sync = "sync"
        def syncError = throw new IllegalStateException("Error getting syncError")
        def async = Future.successful("async")
        def asyncReject: Future[String] = Future.failed(new IllegalStateException("Error getting asyncReject"))
        def asyncError: Future[String] = Future {
          throw new IllegalStateException("Error getting asyncError")
        }
      }

      val schema = Schema(ObjectType("Type", fields[Unit, Data](
        Field("sync", OptionType(StringType))(_.value.sync),
        Field("syncError", OptionType(StringType))(_.value.syncError),
        Field("async", OptionType(StringType))(_.value.async),
        Field("asyncReject", OptionType(StringType))(ctx => ctx.value.asyncReject),
        Field("asyncError", OptionType(StringType))(_.value.asyncError),
        Field("syncDeferError", OptionType(StringType))(
          ctx => DeferredValue(throw new IllegalStateException("Error getting syncDeferError"))),
        Field("asyncDeferError", OptionType(StringType))(
          _ => DeferredFutureValue(Future.failed(throw new IllegalStateException("Error getting asyncDeferError"))))
      )))

      val Success(doc) = QueryParser.parse("""
        {
          sync,
             syncError,
           async,
          asyncReject,
           asyncDeferError
              asyncError
              syncDeferError
        }""")

      val exceptionHandler: PartialFunction[(ResultMarshaller, Throwable), HandledException] = {
        case (m, e: IllegalStateException) => HandledException(e.getMessage)
      }

      val result = Executor(schema, new Data, exceptionHandler = exceptionHandler).execute(doc).await.asInstanceOf[Map[String, Any]]

      val data = result("data")
      val errors = result("errors").asInstanceOf[List[_]]

      data should be (Map(
        "sync" -> "sync",
        "syncError" -> null,
        "async" -> "async",
        "asyncReject" -> null,
        "asyncError" -> null,
        "asyncDeferError" -> null,
        "syncDeferError" -> null
      ))

      errors should (have(size(5)) and
          contain(Map(
            "field" -> "syncError",
            "locations" -> List(Map("line" -> 4, "column" -> 14)),
            "message" -> "Error getting syncError")) and
          contain(Map(
            "field" -> "asyncReject",
            "locations" -> List(Map("line" -> 6, "column" -> 11)),
            "message" -> "Error getting asyncReject")) and
          contain(Map(
            "message" -> "Error getting asyncDeferError",
            "field" -> "asyncDeferError",
            "locations" -> List(Map("line" -> 7, "column" -> 12)))) and
          contain(Map(
            "message" -> "Error getting syncDeferError",
            "field" -> "syncDeferError",
            "locations" -> List(Map("line" -> 9, "column" -> 15)))) and
          contain(Map(
            "field" -> "asyncError",
            "locations" -> List(Map("line" -> 8, "column" -> 15)),
            "message" -> "Error getting asyncError")))
    }

    "use the inline operation if no operation is provided" in {
      val schema = Schema(ObjectType("Type", fields[Unit, Unit](
        Field("a", OptionType(StringType))(_ => "b"))))
      val Success(doc) = QueryParser.parse("{ a }")

      Executor(schema).execute(doc).await should be (Map("data" -> Map("a" -> "b")))
    }

    "use the only operation if no operation is provided" in {
      val schema = Schema(ObjectType("Type", fields[Unit, Unit](
        Field("a", OptionType(StringType))(_ => "b"))))
      val Success(doc) = QueryParser.parse("query Example { a }")

      Executor(schema).execute(doc).await should be (Map("data" -> Map("a" -> "b")))
    }

    "throw if no operation is provided with multiple operations" in {
      val schema = Schema(ObjectType("Type", fields[Unit, Unit](
        Field("a", OptionType(StringType))(_ => "b"))))
      val Success(doc) = QueryParser.parse("query Example { a } query OtherExample { a }")

      Executor(schema).execute(doc).await should be (
        Map("data" -> null, "errors" -> List(Map("message" -> "Must provide operation name if query contains multiple operations"))))
    }

    "use the query schema for queries" in {
      val schema = Schema(
        ObjectType("Q", fields[Unit, Unit](Field("a", OptionType(StringType))(_ => "b"))),
        Some(ObjectType("M", fields[Unit, Unit](Field("c", OptionType(StringType))(_ => "d")))))
      val Success(doc) = QueryParser.parse("query Q { a } mutation M { c }")

      Executor(schema).execute(doc, Some("Q")).await should be  (Map("data" -> Map("a" -> "b")))
    }

    "avoid recursion" in {
      val schema = Schema(ObjectType("Type", fields[Unit, Unit](
        Field("a", OptionType(StringType))(_ => "b"))))

      val Success(doc) = QueryParser.parse("""
        query Q {
          a
          ...Frag
          ...Frag
        }

        fragment Frag on Type {
          a,
          ...Frag
        }
      """)

      Executor(schema, queryValidator = QueryValidator.empty).execute(doc, Some("Q")).await should be  (Map("data" -> Map("a" -> "b")))
    }

    "not include illegal fields in output" in {
      val schema = Schema(
        ObjectType("Q", fields[Unit, Unit](Field("a", OptionType(StringType))(_ => "b"))),
        Some(ObjectType("M", fields[Unit, Unit](Field("c", OptionType(StringType))(_ => "d")))))
      val Success(doc) = QueryParser.parse("mutation M { thisIsIllegalDontIncludeMe }")

      Executor(schema, queryValidator = QueryValidator.empty).execute(doc).await should be  (Map("data" -> Map()))
    }

    "update context in query operations" in {
      val Success(doc) = QueryParser.parse("""
        query Q {
          ctxUpdating {
            ctxColor
          }

          ctxUpdatingFut {
            ctxColor
          }

          ctxUpdatingDef {
            ctxColor
          }

          ctxUpdatingDefFut {
            ctxColor
          }
        }
        """)

      val schema = Schema(DataType)

      Executor(schema, new TestSubject, Ctx(), deferredResolver = new LightColorResolver).execute(doc).await should be  (
        Map(
          "data" -> Map(
            "ctxUpdating" -> Map("ctxColor" -> "blue"),
            "ctxUpdatingFut" -> Map("ctxColor" -> "orange"),
            "ctxUpdatingDef" -> Map("ctxColor" -> "lightmagenta"),
            "ctxUpdatingDefFut" -> Map("ctxColor" -> "lightred"))))
    }

    "resolve deferred values correctly" in {
      val Success(doc) = QueryParser.parse("""
        {
          def { color }
          defFut { color }
        }
        """)

      val schema = Schema(DataType)

      Executor(schema, new TestSubject, Ctx(), deferredResolver = new LightColorResolver).execute(doc).await should be  (
        Map(
          "data" -> Map(
            "def" -> Map("color" -> "lightmagenta"),
            "defFut" -> Map("color" -> "lightred"))))
    }

    "resolve deferred values correctly in presence of errors" in {
      val Success(doc) = QueryParser.parse("""
        {
          defFail { color }
          defFutFail { color }
        }
        """)

      val schema = Schema(DataType)

      val exceptionHandler: PartialFunction[(ResultMarshaller, Throwable), HandledException] = {
        case (m, e: IllegalStateException) => HandledException(e.getMessage)
      }

      Executor.execute(schema, doc,
        root = new TestSubject,
        userContext = Ctx(),
        deferredResolver = new LightColorResolver,
        exceptionHandler = exceptionHandler).await should be  (
          Map(
            "data" -> Map(
              "defFail" -> null,
              "defFutFail" -> null),
            "errors" -> List(
              Map(
                "message" -> "error in resolver",
                "field" -> "defFail",
                "locations" -> List(Map("line" -> 3, "column" -> 11))),
              Map(
                "message" -> "error in resolver",
                "field" -> "defFutFail",
                "locations" -> List(Map("line" -> 4, "column" -> 11))))))
    }
  }
}
