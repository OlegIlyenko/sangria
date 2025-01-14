package sangria.marshalling

import java.text.SimpleDateFormat
import java.util.{Calendar, Date, TimeZone}

import sangria.ast
import sangria.execution.Executor
import sangria.schema._
import sangria.macros._
import sangria.util.FutureResultSupport
import sangria.validation.ValueCoercionViolation
import software.amazon.ion.system.IonSystemBuilder

import scala.util.{Failure, Success, Try}
import scala.concurrent.ExecutionContext.Implicits.global
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class IonSupportSpec extends AnyWordSpec with Matchers with FutureResultSupport {

  import sangria.marshalling.ion._

  implicit val ionSystem: software.amazon.ion.IonSystem = IonSystemBuilder.standard().build()

  val dateFormat = new SimpleDateFormat("yyyy-MM-dd")

  case object DateCoercionViolation extends ValueCoercionViolation("Date value expected")
  case object BinaryCoercionViolation
      extends ValueCoercionViolation("Binary data is not supported as input")

  def parseDate(s: String) = Try(dateFormat.parse(s)) match {
    case Success(d) => Right(d)
    case Failure(error) => Left(DateCoercionViolation)
  }

  val DateType = ScalarType[Date](
    "Date",
    coerceOutput = (d, caps) =>
      if (caps.contains(DateSupport)) d
      else dateFormat.format(d),
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

  val BlobType = ScalarType[Array[Byte]](
    "Blob",
    coerceOutput = (d, _) => d,
    coerceUserInput = {
      case bs: Array[Byte] => Right(bs)
      case _ => Left(BinaryCoercionViolation)
    },
    coerceInput = _ => Left(BinaryCoercionViolation)
  )

  val ClobType = ScalarType[Array[Byte]](
    "Clob",
    coerceOutput = (d, _) => d,
    coerceUserInput = {
      case bs: Array[Byte] => Right(bs)
      case _ => Left(BinaryCoercionViolation)
    },
    coerceInput = _ => Left(BinaryCoercionViolation),
    scalarInfo = Set(IonClobScalar)
  )

  lazy val TestType: ObjectType[Unit, Unit] = ObjectType(
    "Test",
    () =>
      fields[Unit, Unit](
        Field("nested", OptionType(TestType), resolve = _ => ()),
        Field(
          "text",
          OptionType(StringType),
          arguments = Argument("toShow", StringType) :: Nil,
          resolve = c => "foo " + c.arg[String]("toShow")),
        Field(
          "date",
          OptionType(DateType),
          resolve = _ => {
            val cal = Calendar.getInstance(TimeZone.getTimeZone("CET"))
            cal.set(2015, 5, 11, 18, 23, 14)
            cal.set(Calendar.MILLISECOND, 123)
            cal.getTime
          }
        ),
        Field("blob", OptionType(BlobType), resolve = _ => "foo bar".getBytes("UTF-8")),
        Field("clob", OptionType(ClobType), resolve = _ => "foo bar baz".getBytes("UTF-8"))
      )
  )

  val schema = Schema(TestType)

  "Ion Support" should {
    "execute complex query" in {
      val query =
        graphql"""
          query Foo($$a: String!, $$b: String!) {
            text(toShow: $$a)
            nested {
              date
              blob
              clob
              text(toShow: $$b)
            }
          }
        """

      val vars =
        """
          {a: "Hello", b: "World"}
        """
      val varsAst = ionSystem.newLoader.load(vars).get(0)

      val expected =
        """
          {
            data: {
              text: "foo Hello",
              nested: {
                date: 2015-06-11T16:23:14.123Z,
                blob: {{Zm9vIGJhcg==}},
                clob: {{"foo bar baz"}},
                text: "foo World"
              }
            }
          }
        """

      Executor.execute(schema, query, variables = varsAst).await should be(
        ionSystem.newLoader.load(expected).get(0))
    }
  }

}
