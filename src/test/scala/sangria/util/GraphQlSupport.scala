package sangria.util

import org.scalatest.Matchers
import sangria.execution.{HandledException, InputUnmarshaller, Executor, ResultMarshaller}
import sangria.parser.QueryParser
import sangria.schema.{DeferredResolver, Schema}
import sangria.validation.QueryValidator
import spray.json.{JsValue, JsObject}

import scala.util.Success

import scala.concurrent.ExecutionContext.Implicits.global
import sangria.integration.SprayJsonSupport.SprayJsonInputUnmarshaller

trait GraphQlSupport extends AwaitSupport with Matchers {
  def schema: Schema[_, _]

  def executeTestQuery[T, A: InputUnmarshaller](data: T, query: String, args: Option[A], userContext: Any = (), resolver: DeferredResolver = DeferredResolver.empty) = {
    val Success(doc) = QueryParser.parse(query)

    val exceptionHandler: PartialFunction[(ResultMarshaller, Throwable), HandledException] = {
      case (m, e: IllegalStateException) => HandledException(e.getMessage)
    }

    Executor(
      schema.asInstanceOf[Schema[Any, T]],
      data,
      exceptionHandler = exceptionHandler,
      userContext = userContext,
      queryValidator = QueryValidator.empty,
      deferredResolver = resolver).execute(doc.copy(sourceMapper = None), arguments = args).await
  }

  def check[T](data: T, query: String, expected: Any, args: Option[JsValue] = None, userContext: Any = (), resolver: DeferredResolver = DeferredResolver.empty) = {
    executeTestQuery(data, query, args, userContext, resolver) should be (expected)
  }

  def checkErrors[T](data: T, query: String, expectedData: Map[String, Any], expectedErrors: List[Map[String, Any]], args: Option[JsValue] = None, userContext: Any = (), resolver: DeferredResolver = DeferredResolver.empty) = {
    val result = executeTestQuery(data, query, args, userContext, resolver).asInstanceOf[Map[String, Any]]

    result("data") should be (expectedData)

    val errors = result.getOrElse("errors", Nil).asInstanceOf[List[Any]]

    errors should have size expectedErrors.size

    expectedErrors foreach (expected => errors should contain (expected))
  }

  def checkContainsErrors[T](data: T, query: String, expectedData: Map[String, Any], expectedErrorStrings: List[(String, Option[Pos])], args: Option[JsValue] = None) = {
    val result = executeTestQuery(data, query, args).asInstanceOf[Map[String, Any]]

    result("data") should be (expectedData)

    val errors = result.getOrElse("errors", Nil).asInstanceOf[List[Map[String, Any]]]

    errors should have size expectedErrorStrings.size

    expectedErrorStrings foreach { case(expected, pos) =>
      withClue(s"Expected error not found: $expected${pos map (p => s" (line ${p.line}, column ${p.col})") getOrElse ""}. Actual:\n$errors") {
        errors exists { error =>
          val message = error("message").asInstanceOf[String]

          message.contains(expected) && {
            pos map { p =>
              val location = error("locations").asInstanceOf[List[Map[String, Any]]](0)

              location("line") == p.line && location("column") == p.col
            } getOrElse true
          }
        } should be(true)
      }
    }
  }
}

case class Pos(line: Int, col: Int)