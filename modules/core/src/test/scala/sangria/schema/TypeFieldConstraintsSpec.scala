package sangria.schema

import sangria.util.Pos
import sangria.util.SimpleGraphQlSupport._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import sangria.util.tag.@@ // Scala 3 issue workaround
import sangria.marshalling.FromInput.CoercedScalaResult

class TypeFieldConstraintsSpec extends AnyWordSpec with Matchers {

  "ObjectType" should {
    "allow unique fields" in {
      ObjectType(
        "Test",
        fields[Unit, Unit](
          Field("a", StringType, resolve = _ => "foo"),
          Field("b", StringType, resolve = _ => "foo"),
          Field("c", StringType, resolve = _ => "foo")
        ))

      ObjectType(
        "Test",
        () =>
          fields[Unit, Unit](
            Field("a", StringType, resolve = _ => "foo"),
            Field("b", StringType, resolve = _ => "foo"),
            Field("c", StringType, resolve = _ => "foo")
          )).fields
    }

    "disallow non-unique fields" in {
      val e = intercept[SchemaValidationException] {
        Schema(
          ObjectType(
            "Test",
            fields[Unit, Unit](
              Field("a", StringType, resolve = _ => "foo"),
              Field("b", StringType, resolve = _ => "foo"),
              Field("a", StringType, resolve = _ => "foo")
            )))
      }

      e.getMessage should include("Object type 'Test' can include field 'a' only once.")
    }

    "disallow invalid names" in {
      an[SchemaValidationException] should be thrownBy
        Schema(
          ObjectType(
            "Test-object",
            fields[Unit, Unit](
              Field("a", StringType, resolve = _ => "foo")
            )))
    }
  }

  "InterfaceType" should {
    "allow unique fields" in {
      InterfaceType(
        "Test",
        fields[Unit, Unit](
          Field("a", StringType, resolve = _ => "foo"),
          Field("b", StringType, resolve = _ => "foo"),
          Field("c", StringType, resolve = _ => "foo")
        ))

      InterfaceType(
        "Test",
        () =>
          fields[Unit, Unit](
            Field("a", StringType, resolve = _ => "foo"),
            Field("b", StringType, resolve = _ => "foo"),
            Field("c", StringType, resolve = _ => "foo")
          )).fields
    }

    "disallow non-unique fields" in {
      an[SchemaValidationException] should be thrownBy {
        val TestType = InterfaceType(
          "Test",
          fields[Unit, Unit](
            Field("a", StringType, resolve = _ => "foo"),
            Field("b", StringType, resolve = _ => "foo"),
            Field("a", StringType, resolve = _ => "foo")
          ))

        Schema(
          ObjectType(
            "Foo",
            interfaces[Unit, Unit](TestType),
            fields[Unit, Unit](
              Field("d", StringType, resolve = _ => "foo")
            )))
      }
    }

    "disallow invalid names" in {
      an[SchemaValidationException] should be thrownBy {
        val TestType = InterfaceType(
          "Test-int",
          fields[Unit, Unit](
            Field("a", StringType, resolve = _ => "foo")
          ))

        Schema(
          ObjectType(
            "Foo",
            interfaces[Unit, Unit](TestType),
            fields[Unit, Unit](
              Field("d", StringType, resolve = _ => "foo")
            )))
      }
    }
  }

  "InputObjectType" should {
    "allow unique fields" in {
      InputObjectType(
        "Test",
        List(
          InputField("a", StringType),
          InputField("b", StringType),
          InputField("c", StringType)
        ))

      InputObjectType(
        "Test",
        () =>
          List(
            InputField("a", StringType),
            InputField("b", StringType),
            InputField("c", StringType)
          )).fields
    }

    "disallow non-unique fields" in {
      an[SchemaValidationException] should be thrownBy {
        val TestType = InputObjectType(
          "Test",
          List(
            InputField("a", StringType),
            InputField("b", StringType),
            InputField("a", StringType)
          ))

        Schema(
          ObjectType(
            "Foo",
            fields[Unit, Unit](
              Field(
                "d",
                StringType,
                arguments = Argument("test", TestType) :: Nil,
                resolve = _ => "foo")
            )))
      }
    }

    "disallow invalid names" in {
      an[SchemaValidationException] should be thrownBy {
        val TestType = InputObjectType(
          "Test-ab",
          List(
            InputField("a", StringType),
            InputField("b", StringType),
            InputField("a", StringType)
          ))

        Schema(
          ObjectType(
            "Foo",
            fields[Unit, Unit](
              Field(
                "d",
                StringType,
                arguments = Argument("test", TestType) :: Nil,
                resolve = _ => "foo")
            )))
      }

      an[SchemaValidationException] should be thrownBy {
        val TestType = InputObjectType(
          "Test",
          List(
            InputField("a-b", StringType)
          ))

        Schema(
          ObjectType(
            "Foo",
            fields[Unit, Unit](
              Field(
                "d",
                StringType,
                arguments = Argument("test", TestType) :: Nil,
                resolve = _ => "foo")
            )))
      }
    }
  }

  case class A(b: Option[B], name: String)
  case class B(a: A, size: Int)

  val AType: ObjectType[Unit, A] = ObjectType(
    "A",
    fields[Unit, A](
      Field("name", StringType, resolve = _.value.name),
      Field("b", OptionType(BType), resolve = _.value.b)))

  lazy val BType: ObjectType[Unit, B] = ObjectType(
    "B",
    fields[Unit, B](
      Field("size", IntType, resolve = _.value.size),
      Field("a", AType, resolve = _.value.a)))

  "Schema" should {
    "provide a helpful error message if circular references are detected" in {
      val QueryType = ObjectType(
        "Query",
        fields[Unit, Unit](
          Field("a", AType, resolve = _ => A(Some(B(A(None, "bar"), 1)), "foo"))
        ))

      val error = intercept[IllegalStateException](Schema(QueryType))

      error.getMessage should include(
        "A `null` value was provided instead of type for a field 'a' of 'B' type.")
    }

    "ensure that implemented fields have correct type" in {
      val FruitType = InterfaceType(
        "Fruit",
        fields[Unit, Unit](
          Field("size", IntType, resolve = _ => 1)
        ))

      val SomeOtherInterfaceType = InterfaceType(
        "SomeOtherInterfaceType",
        fields[Unit, Unit](
          Field("size", IntType, resolve = _ => 1)
        ))

      val AppleType = ObjectType(
        "Apple",
        interfaces[Unit, Unit](FruitType),
        fields[Unit, Unit](
          Field("size", IntType, resolve = _ => 1),
          Field("color", StringType, resolve = _ => "red")
        ))

      val BasketType = InterfaceType(
        "Basket",
        fields[Unit, Unit](
          Field("fruit", FruitType, resolve = _ => ())
        ))

      val AppleBasketType = ObjectType(
        "AppleBasket",
        interfaces[Unit, Unit](BasketType),
        fields[Unit, Unit](
          Field("fruit", SomeOtherInterfaceType, resolve = _ => ())
        ))

      val QueryType = ObjectType(
        "Query",
        fields[Unit, Unit](
          Field("basket", BasketType, resolve = _ => ())
        ))

      val error = intercept[SchemaValidationException](
        Schema(QueryType, additionalTypes = AppleType :: AppleBasketType :: Nil))

      error.violations.map(_.errorMessage).head should include(
        "Basket.fruit expects type 'Fruit!', but AppleBasket.fruit provides type 'SomeOtherInterfaceType!'.")
    }

    "ensure that all interface field arguments are present in the implementation" in {
      val FruitType = InterfaceType(
        "Fruit",
        fields[Unit, Unit](
          Field("size", IntType, resolve = _ => 1),
          Field(
            "slice",
            IntType,
            arguments = Argument("parts", IntType) :: Nil,
            resolve = _.args.arg[Int]("parts"))
        )
      )

      val AppleType = ObjectType(
        "Apple",
        interfaces[Unit, Unit](FruitType),
        fields[Unit, Unit](
          Field("size", IntType, resolve = _ => 1),
          Field(
            "slice",
            IntType,
            arguments = Argument("pieces", IntType) :: Nil,
            resolve = _.args.arg[Int]("pieces"))
        )
      )

      val QueryType = ObjectType(
        "Query",
        fields[Unit, Unit](
          Field("fruit", FruitType, resolve = _ => ())
        ))

      val error =
        intercept[SchemaValidationException](Schema(QueryType, additionalTypes = AppleType :: Nil))

      error.violations.last.errorMessage should include(
        "Fruit.slice expects argument 'parts', but Apple.slice does not provide it.")
    }

    "ensure that all interface field argument types are the same in the implementation" in {
      val FruitType = InterfaceType(
        "Fruit",
        fields[Unit, Unit](
          Field("size", IntType, resolve = _ => 1),
          Field(
            "slice",
            IntType,
            arguments = Argument("parts", IntType) :: Nil,
            resolve = _.args.arg[Int]("parts"))
        )
      )

      val AppleType = ObjectType(
        "Apple",
        interfaces[Unit, Unit](FruitType),
        fields[Unit, Unit](
          Field("size", IntType, resolve = _ => 1),
          Field(
            "slice",
            IntType,
            arguments = Argument("parts", StringType) :: Nil,
            resolve = _.args.arg[String]("parts").toInt)
        )
      )

      val QueryType = ObjectType(
        "Query",
        fields[Unit, Unit](
          Field("fruit", FruitType, resolve = _ => ())
        ))

      val error =
        intercept[SchemaValidationException](Schema(QueryType, additionalTypes = AppleType :: Nil))

      error.violations.head.errorMessage should include(
        "Fruit.slice(parts) expects type 'Int!', but Apple.slice(parts) provides type 'String!'.")
    }

    "ensure that all implementation extra field arguments are optional" in {
      val FruitType = InterfaceType(
        "Fruit",
        fields[Unit, Unit](
          Field("size", IntType, resolve = _ => 1),
          Field(
            "slice",
            IntType,
            arguments = Argument("parts", IntType) :: Nil,
            resolve = _.args.arg[Int]("parts"))
        )
      )

      val AppleType = ObjectType(
        "Apple",
        interfaces[Unit, Unit](FruitType),
        fields[Unit, Unit](
          Field("size", IntType, resolve = _ => 1),
          Field(
            "slice",
            IntType,
            arguments = Argument("parts", IntType) :: Argument("careful", BooleanType) :: Nil,
            resolve = _.args.arg[Int]("parts"))
        )
      )

      val QueryType = ObjectType(
        "Query",
        fields[Unit, Unit](
          Field("fruit", FruitType, resolve = _ => ())
        ))

      val error =
        intercept[SchemaValidationException](Schema(QueryType, additionalTypes = AppleType :: Nil))

      error.violations.head.errorMessage should include(
        "Apple.slice(careful) is of required type 'Boolean!', but is not also provided by the interface Fruit.slice.")
    }

    "allow covariant return types" in {
      trait Fruit {
        def size: Int
      }

      case class Apple(size: Int, color: String) extends Fruit

      trait Basket {
        def fruit: Fruit
      }

      case class AppleBasket(fruit: Apple) extends Basket

      val FruitType = InterfaceType(
        "Fruit",
        fields[Unit, Fruit](
          Field("size", IntType, resolve = _.value.size),
          Field(
            "slice",
            IntType,
            arguments = Argument("parts", IntType) :: Nil,
            resolve = _.args.arg[Int]("parts"))
        )
      )

      val AppleType = ObjectType(
        "Apple",
        interfaces[Unit, Apple](FruitType),
        fields[Unit, Apple](
          Field("size", IntType, resolve = _.value.size),
          Field(
            "slice",
            IntType,
            arguments =
              Argument("parts", IntType) :: Argument[Option[Boolean @@ CoercedScalaResult]](
                "careful",
                OptionInputType(BooleanType)) :: Nil,
            resolve = _.args.arg[Int]("parts")
          ),
          Field("color", StringType, resolve = _.value.color)
        )
      )

      val BasketType = InterfaceType(
        "Basket",
        fields[Unit, Basket](
          Field("fruit", FruitType, resolve = _.value.fruit)
        ))

      val AppleBasketType = ObjectType(
        "AppleBasket",
        interfaces[Unit, AppleBasket](BasketType),
        fields[Unit, AppleBasket](
          Field("fruit", AppleType, resolve = _.value.fruit)
        ))

      val QueryType = ObjectType(
        "Query",
        fields[Unit, Basket](
          Field("basket", BasketType, resolve = _.value)
        ))

      val schema = Schema(QueryType, additionalTypes = AppleType :: AppleBasketType :: Nil)

      check(
        schema,
        AppleBasket(Apple(11, "red")),
        """
         {
          basket {
            fruit {
              size
              slice(parts: 5)
            }
          }
         }
       """,
        Map("data" -> Map("basket" -> Map("fruit" -> Map("size" -> 11, "slice" -> 5))))
      )

      checkContainsErrors(
        schema,
        AppleBasket(Apple(11, "red")),
        """
         {
          basket {
            fruit {
              color
            }
          }
         }
        """,
        None,
        List(
          "Cannot query field 'color' on type 'Fruit'. Did you mean to use an inline fragment on 'Apple'?" -> List(
            Pos(5, 15)))
      )
    }
  }

}
