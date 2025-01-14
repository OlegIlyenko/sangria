package sangria.schema

import java.text.SimpleDateFormat
import java.util.Date

import sangria.ast
import sangria.macros._
import sangria.util.Pos
import sangria.util.SimpleGraphQlSupport._
import sangria.validation.ValueCoercionViolation

import scala.util.{Failure, Success, Try}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import sangria.validation.BigDecimalCoercionViolation

class CustomScalarSpec extends AnyWordSpec with Matchers {
  "Schema" should {
    "allow to define custom scalar types" in {
      val dateFormat = new SimpleDateFormat("yyyy-MM-dd")

      case object DateCoercionViolation extends ValueCoercionViolation("Date value expected")

      def parseDate(s: String) = Try(dateFormat.parse(s)) match {
        case Success(d) => Right(d)
        case Failure(error) => Left(DateCoercionViolation)
      }

      val DateType = ScalarType[Date](
        "Date",
        description = Some("An example of date scalar type"),
        coerceOutput = (d, _) => dateFormat.format(d),
        coerceUserInput = {
          case s: String => parseDate(s)
          case d: Date => Right(d)
          case _ => Left(DateCoercionViolation)
        },
        coerceInput = {
          case ast.StringValue(s, _, _, _, _) => parseDate(s)
          case _ => Left(DateCoercionViolation)
        }
      )

      val DateArg = Argument("dateInput", DateType)

      val QueryType = ObjectType(
        "Query",
        fields[Unit, Unit](
          Field(
            "foo",
            DateType,
            arguments = DateArg :: Nil,
            resolve = ctx => {
              val date: Date = ctx.arg(DateArg)
              new Date(date.getTime + 1000 * 60 * 60 * 24 * 5)
            })
        )
      )

      val schema = Schema(QueryType)

      check(
        schema,
        (),
        """
          {
            foo(dateInput: "2015-05-11")
          }
        """,
        Map("data" -> Map("foo" -> "2015-05-16")))

      checkContainsErrors(
        schema,
        (),
        """
          {
            foo(dateInput: "2015-05-test")
          }
        """,
        None,
        List(
          """Expected type 'Date!', found '"2015-05-test"'. Date value expected""" -> List(
            Pos(3, 28)))
      )
    }

    "allow to overwrite built-in scalar type" in {

      def parseBigDecimal(s: String) = Try(BigDecimal(s)) match {
        case Success(d) => Right(d)
        case Failure(error) => Left(BigDecimalCoercionViolation)
      }

      val BigDecimalType = ScalarType[BigDecimal](
        "BigDecimal",
        description = Some("A string only BigDecimal type"),
        coerceOutput = (d, _) => d.toString,
        coerceUserInput = {
          case s: String => parseBigDecimal(s)
          case _ => Left(BigDecimalCoercionViolation)
        },
        coerceInput = {
          case ast.StringValue(s, _, _, _, _) => parseBigDecimal(s)
          case _ => Left(BigDecimalCoercionViolation)
        }
      )

      val schemaSdl = graphql"""
        scalar BigDecimal

        type Query {
          age: BigDecimal!
        }
      """

      val schema = Schema.buildFromAst[Unit](
        schemaSdl,
        AstSchemaBuilder.resolverBased(
          ScalarResolver[Unit] { case ast.ScalarTypeDefinition("BigDecimal", _, _, _, _) =>
            BigDecimalType
          }
        )
      )

      schema.scalarTypes.collectFirst { case ("BigDecimal", tpe) => tpe } should be(
        Some(BigDecimalType))

    }
  }
}
