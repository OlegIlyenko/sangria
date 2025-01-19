package sangria.schema

import sangria.util.{GraphQlSupport, Pos}
import spray.json.{JsNumber, JsObject, JsString}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class EnumTypeSpec extends AnyWordSpec with Matchers with GraphQlSupport {
  val ColorType = EnumType(
    "Color",
    values = List(
      EnumValue("RED", value = 0),
      EnumValue("GREEN", value = 1),
      EnumValue("BLUE", value = 2)
    ))

  object Args {
    val fromEnum: Argument[Option[Int]] = Argument("fromEnum", OptionInputType(ColorType))
    val fromInt: Argument[Option[Int]] = Argument("fromInt", OptionInputType(IntType))
  }

  val QueryType = ObjectType(
    "Query",
    fields[Unit, Unit](
      Field(
        "colorEnum",
        OptionType(ColorType),
        arguments = Args.fromEnum :: Args.fromInt :: Nil,
        resolve = ctx => ctx.args.arg(Args.fromInt).orElse(ctx.args.arg(Args.fromEnum))
      ),
      Field(
        "colorInt",
        OptionType(IntType),
        arguments = Args.fromEnum :: Args.fromInt :: Nil,
        resolve = ctx => ctx.args.arg(Args.fromInt).orElse(ctx.args.arg(Args.fromEnum))
      )
    )
  )

  val MutationType = ObjectType(
    "Mutation",
    fields[Unit, Unit](
      Field(
        "favoriteEnum",
        OptionType(ColorType),
        arguments = Args.fromEnum :: Nil,
        resolve = _.args.arg[Option[Int]](Args.fromEnum))
    )
  )

  val SubscriptionType = ObjectType(
    "Subscription",
    fields[Unit, Unit](
      Field(
        "subscribeToEnum",
        OptionType(ColorType),
        arguments = Args.fromEnum :: Nil,
        resolve = _.args.arg[Option[Int]](Args.fromEnum))
    )
  )

  val schema = Schema(QueryType, Some(MutationType), Some(SubscriptionType))

  "Type System: Enum Values" should {
    "accepts enum literals as input" in check(
      (),
      "{ colorInt(fromEnum: GREEN) }",
      Map("data" -> Map("colorInt" -> 1)))

    "enum may be output type" in check(
      (),
      "{ colorEnum(fromInt: 1) }",
      Map("data" -> Map("colorEnum" -> "GREEN")))

    "enum may be both input and output type" in check(
      (),
      "{ colorEnum(fromEnum: GREEN) }",
      Map("data" -> Map("colorEnum" -> "GREEN")))

    "does not accept string literals" in checkContainsErrors(
      (),
      """{ colorEnum(fromEnum: "GREEN") }""",
      Some(Map("colorEnum" -> null)),
      List(
        "Argument 'fromEnum' has wrong value: Enum value expected." -> List(Pos(1, 3), Pos(1, 23))),
      validateQuery = false
    )

    "does not accept internal value in place of enum literal" in checkContainsErrors(
      (),
      """{ colorEnum(fromEnum: 1) }""",
      Some(Map("colorEnum" -> null)),
      List(
        "Argument 'fromEnum' has wrong value: Enum value expected." -> List(Pos(1, 3), Pos(1, 23))),
      validateQuery = false
    )

    "accepts JSON string as enum variable" in check(
      (),
      """query test($color: Color!) { colorEnum(fromEnum: $color) }""",
      Map("data" -> Map("colorEnum" -> "BLUE")),
      JsObject("color" -> JsString("BLUE")))

    "accepts enum literals as input arguments to mutations" in check(
      (),
      """mutation x($color: Color!) { favoriteEnum(fromEnum: $color) }""",
      Map("data" -> Map("favoriteEnum" -> "GREEN")),
      JsObject("color" -> JsString("GREEN"))
    )

    "accepts enum literals as input arguments to subscriptions" in check(
      (),
      """subscription x($color: Color!) { subscribeToEnum(fromEnum: $color) }""",
      Map("data" -> Map("subscribeToEnum" -> "GREEN")),
      JsObject("color" -> JsString("GREEN"))
    )

    "not accept internal value as enum variables" in checkContainsErrors(
      (),
      """query test($color: Color!) { colorEnum(fromEnum: $color) }""",
      None,
      List(
        "Variable '$color' expected value of type 'Color!' but got: 1. Reason: Enum value expected" -> List(
          Pos(1, 12))),
      JsObject("color" -> JsNumber(1))
    )

    "not accept string variables as enum input" in checkContainsErrors(
      (),
      """query test($color: String!) { colorEnum(fromEnum: $color) }""",
      None,
      List(
        "Variable '$color' of type 'String!' used in position expecting type 'Color'." -> List(
          Pos(1, 12),
          Pos(1, 51))),
      JsObject("color" -> JsString("BLUE")),
      validateQuery = true
    )

    "not accept internal value variable as enum input" in checkContainsErrors(
      (),
      """query test($color: Int!) { colorEnum(fromEnum: $color) }""",
      None,
      List(
        "Variable '$color' of type 'Int!' used in position expecting type 'Color'." -> List(
          Pos(1, 12),
          Pos(1, 48))),
      JsObject("color" -> JsNumber(2)),
      validateQuery = true
    )

    "enum value may have an internal value of 0" in check(
      (),
      """
       {
         colorEnum(fromEnum: RED)
         colorInt(fromEnum: RED)
       }
      """,
      Map("data" -> Map("colorEnum" -> "RED", "colorInt" -> 0))
    )

    "enum inputs may be nullable" in check(
      (),
      """
       {
         colorEnum
         colorInt
       }
      """,
      Map("data" -> Map("colorEnum" -> null, "colorInt" -> null)))
  }
}
