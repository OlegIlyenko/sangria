package sangria.execution

import sangria.parser.QueryParser
import sangria.schema._
import sangria.macros._
import sangria.util.{FutureResultSupport, OutputMatchers, StringMatchers}
import sangria.validation.{
  AstNodeLocation,
  BadValueViolation,
  BaseViolation,
  UndefinedFieldViolation
}
import sangria.marshalling.MarshallingUtil._

import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ExceptionHandlingSpec
    extends AnyWordSpec
    with Matchers
    with FutureResultSupport
    with OutputMatchers
    with StringMatchers {

  case object EmailTypeViolation extends BaseViolation("Invalid email")

  val errorScalar = ScalarAlias[String, String](
    StringType,
    toScalar = identity,
    fromScalar = _ => Left(EmailTypeViolation))

  val TestType = ObjectType(
    "Test",
    fields[Unit, Unit](
      Field(
        "success",
        OptionType(StringType),
        arguments = List(Argument("num", OptionInputType(IntType))),
        resolve = _ => "Yay"),
      Field(
        "errorInScalar",
        OptionType(StringType),
        arguments = Argument("email", errorScalar) :: Nil,
        resolve = _ => "Yay"),
      Field("trySuccess", OptionType(StringType), resolve = _ => Success("try!")),
      Field(
        "tryError",
        OptionType(StringType),
        resolve = _ => Failure(new IllegalStateException("try boom!"))),
      Field(
        "error",
        OptionType(StringType),
        resolve = _ => throw new IllegalStateException("Boom!")),
      Field(
        "futureError",
        OptionType(StringType),
        resolve = _ => Future.failed[String](new IllegalStateException("Boom!")))
    )
  )

  val schema = Schema(TestType)

  "Exception handling" should {
    "obfuscate unexpected exceptions" in {
      val out = captureStdErr {
        val Success(doc) = QueryParser.parse("""
        {
          success
          tryError
          trySuccess
          error
          futureError
        }
        """)

        Executor.execute(schema, doc).await should be(
          Map(
            "data" -> Map(
              "success" -> "Yay",
              "trySuccess" -> "try!",
              "tryError" -> null,
              "error" -> null,
              "futureError" -> null),
            "errors" -> List(
              Map(
                "message" -> "Internal server error",
                "path" -> List("error"),
                "locations" -> List(Map("line" -> 6, "column" -> 11))),
              Map(
                "message" -> "Internal server error",
                "path" -> List("tryError"),
                "locations" -> List(Map("line" -> 4, "column" -> 11))),
              Map(
                "message" -> "Internal server error",
                "path" -> List("futureError"),
                "locations" -> List(Map("line" -> 7, "column" -> 11)))
            )
          ))
      }

      out should include("java.lang.IllegalStateException: Boom!")
    }

    "provide user-defined exception handling mechanism" in {
      val Success(doc) = QueryParser.parse("""
        {
          error
          futureError
        }
        """)

      val exceptionHandler = ExceptionHandler { case (m, e: IllegalStateException) =>
        HandledException(e.getMessage)
      }

      Executor.execute(schema, doc, exceptionHandler = exceptionHandler).await should be(
        Map(
          "data" -> Map("error" -> null, "futureError" -> null),
          "errors" -> List(
            Map(
              "message" -> "Boom!",
              "path" -> List("error"),
              "locations" -> List(Map("line" -> 3, "column" -> 11))),
            Map(
              "message" -> "Boom!",
              "path" -> List("futureError"),
              "locations" -> List(Map("line" -> 4, "column" -> 11)))
          )
        ))
    }

    "provide user-defined exception handling mechanism which allows to provide additional fields" in {
      val Success(doc) = QueryParser.parse("""
        {
          error
          futureError
        }
        """)

      val exceptionHandler = ExceptionHandler { case (m, e: IllegalStateException) =>
        HandledException(
          e.getMessage,
          Map("foo" -> m.list(m.fromString("bar"), m.fromInt(1234)), "baz" -> m.fromString("Test")))
      }

      Executor.execute(schema, doc, exceptionHandler = exceptionHandler).await should be(
        Map(
          "data" -> Map("error" -> null, "futureError" -> null),
          "errors" -> List(
            Map(
              "message" -> "Boom!",
              "path" -> List("error"),
              "locations" -> List(Map("line" -> 3, "column" -> 11)),
              "extensions" -> Map("foo" -> List("bar", 1234), "baz" -> "Test")),
            Map(
              "message" -> "Boom!",
              "path" -> List("futureError"),
              "locations" -> List(Map("line" -> 4, "column" -> 11)),
              "extensions" -> Map("foo" -> List("bar", 1234), "baz" -> "Test")
            )
          )
        ))
    }

    // https://spec.graphql.org/October2021/#sec-Data
    "the data entry is not be present if an error was raised before execution begins" in {
      val query = gql"""
          query GetShoppingBag($$id: String!) {
            cart(id: $$id) {
              version
            }
          }
        """

      val res =
        Executor.execute(schema, query).recover { case analysis: QueryAnalysisError =>
          analysis.resolveError
        }

      res.await should be(
        Map(
          "errors" -> Vector(
            Map(
              "message" -> "Cannot query field 'cart' on type 'Test'. (line 3, column 13):\n            cart(id: $id) {\n            ^",
              "locations" -> Vector(Map("line" -> 3, "column" -> 13))
            )
          )
        ))
    }

    "handle violation-based errors" in {
      val Success(doc) = QueryParser.parse("""
        {
          nonExistingField
          success(num: "One")
          errorInScalar(email: "foo")
        }
        """)

      val exceptionHandler = ExceptionHandler(onViolation = {
        case (m, BadValueViolation(_, _, Some(v: EmailTypeViolation.type), _, _)) =>
          HandledException(
            "Scalar",
            Map("original" -> m.scalarNode(v.errorMessage, "String", Set.empty)))
        case (m, v: UndefinedFieldViolation) =>
          HandledException(
            "Field is missing!!! D:",
            Map("fieldName" -> m.scalarNode(v.fieldName, "String", Set.empty)))
        case (_, v: AstNodeLocation) =>
          HandledException(v.simpleErrorMessage + " [with extras]")
      })

      val res =
        Executor.execute(schema, doc, exceptionHandler = exceptionHandler).recover {
          case analysis: QueryAnalysisError => analysis.resolveError
        }

      res.await should be(
        Map(
          "errors" -> Vector(
            Map(
              "message" -> "Field is missing!!! D:",
              "locations" -> Vector(Map("line" -> 3, "column" -> 11)),
              "extensions" -> Map("fieldName" -> "nonExistingField")),
            Map(
              "message" -> "Expected type 'Int', found '\"One\"'. Int value expected [with extras]",
              "locations" -> Vector(Map("line" -> 4, "column" -> 24))),
            Map(
              "message" -> "Scalar",
              "locations" -> Vector(Map("line" -> 5, "column" -> 32)),
              "extensions" -> Map("original" -> "Invalid email"))
          )
        ))
    }

    "handle user-facing errors errors" in {
      val Success(doc) = QueryParser.parse("""
        query Foo {
          success(num: 1)
        }
        """)

      val exceptionHandler =
        ExceptionHandler(onUserFacingError = { case (m, e: OperationSelectionError) =>
          HandledException(
            "Wrong operation?!",
            Map("errorCode" -> m.scalarNode("AAAAAaaAA!", "String", Set.empty)))
        })

      val res =
        Executor
          .execute(schema, doc, operationName = Some("Bar"), exceptionHandler = exceptionHandler)
          .recover { case analysis: QueryAnalysisError =>
            analysis.resolveError
          }

      res.await should be(
        Map("errors" -> Vector(
          Map("message" -> "Wrong operation?!", "extensions" -> Map("errorCode" -> "AAAAAaaAA!")))))
    }

    "allow multiple handled errors with ast positions" in {
      val Success(doc) = QueryParser.parse("""
        query Foo {
          error
        }
        """.stripCR)

      val exceptionHandler = ExceptionHandler { case (m, e: IllegalStateException) =>
        HandledException.multiple(
          Vector(
            ("Error 1", Map("errorCode" -> m.scalarNode("OOPS", "String", Set.empty)), Nil),
            ("Error 2", Map.empty[String, m.Node], doc.operations.head._2.location.toList)
          ))
      }

      Executor.execute(schema, doc, exceptionHandler = exceptionHandler).await should be(
        Map(
          "data" -> Map("error" -> null),
          "errors" -> Vector(
            Map(
              "message" -> "Error 1",
              "path" -> Vector("error"),
              "locations" -> Vector(Map("line" -> 3, "column" -> 11)),
              "extensions" -> Map("errorCode" -> "OOPS")),
            Map(
              "message" -> "Error 2",
              "path" -> Vector("error"),
              "locations" -> Vector(
                Map("line" -> 3, "column" -> 11),
                Map("line" -> 2, "column" -> 9)))
          )
        ))
    }

    "provide a way to add extension fields in the error itself (backwards compat)" in {
      val exceptionHandler = ExceptionHandler { case (m, e: IllegalStateException) =>
        HandledException(
          "Wrong operation?!",
          Map("errorCode" -> m.fromString("Ooops!")),
          addFieldsInError = true)
      }

      Executor.execute(schema, gql"{error}", exceptionHandler = exceptionHandler).await should be(
        Map(
          "data" -> Map("error" -> null),
          "errors" -> Vector(Map(
            "message" -> "Wrong operation?!",
            "path" -> Vector("error"),
            "locations" -> Vector(Map("line" -> 1, "column" -> 2)),
            "errorCode" -> "Ooops!",
            "extensions" -> Map("errorCode" -> "Ooops!")
          ))
        ))
    }

    "provide a way to remove extension fields (backwards compat)" in {
      val exceptionHandler = ExceptionHandler { case (m, e: IllegalStateException) =>
        HandledException(
          "Wrong operation?!",
          Map("errorCode" -> m.fromString("Ooops!")),
          addFieldsInError = true,
          addFieldsInExtensions = false)
      }

      Executor.execute(schema, gql"{error}", exceptionHandler = exceptionHandler).await should be(
        Map(
          "data" -> Map("error" -> null),
          "errors" -> Vector(
            Map(
              "message" -> "Wrong operation?!",
              "path" -> Vector("error"),
              "locations" -> Vector(Map("line" -> 1, "column" -> 2)),
              "errorCode" -> "Ooops!"))
        ))
    }
  }
}
