package sangria.schema

import sangria.ast
import sangria.marshalling.ScalaInput.scalaInput
import sangria.validation.IntCoercionViolation

import scala.concurrent.ExecutionContext.Implicits.global

import sangria.execution.Executor
import sangria.util.{FutureResultSupport, Pos}
import sangria.introspection.{
  IntrospectionField,
  IntrospectionScalarType,
  IntrospectionType,
  introspectionQuery
}
import sangria.util.SimpleGraphQlSupport.{check, checkContainsErrors}
import spray.json._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import sangria.util.StringMatchers
import sangria.util.tag.@@
import sangria.marshalling.FromInput.CoercedScalaResult
import sangria.marshalling.ScalaInput
import sangria.schema.IntType

class IntrospectionSchemaMaterializerSpec
    extends AnyWordSpec
    with Matchers
    with FutureResultSupport
    with StringMatchers {

  // Test property:
  // Given a server's schema, a client may query that server with introspection,
  // and use the result to produce a client-side representation of the schema
  // by using "buildClientSchema". If the client then runs the introspection
  // query against the client-side schema, it should get a result identical to
  // what was returned by the server.
  def testSchema(serverSchema: Schema[Any, Any]): Schema[Any, Any] = {
    import sangria.marshalling.queryAst._

    val initialIntrospection =
      Executor.execute(serverSchema, introspectionQuery(true, true, true)).await
    val clientSchema = IntrospectionSchemaMaterializer.buildSchema(initialIntrospection)
    val secondIntrospection =
      Executor.execute(clientSchema, introspectionQuery(true, true, true)).await

    initialIntrospection should be(secondIntrospection)

    clientSchema
  }

  lazy val RecursiveType: ObjectType[Any, Any] = ObjectType(
    "Recur",
    () =>
      fields(
        Field("recur", OptionType(RecursiveType), resolve = _ => None)
      ))

  lazy val DogType: ObjectType[Any, Any] = ObjectType(
    "Dog",
    () =>
      fields(
        Field("bestFriend", OptionType(HumanType), resolve = _ => None)
      ))

  lazy val HumanType: ObjectType[Any, Any] = ObjectType(
    "Human",
    () =>
      fields(
        Field("bestFriend", OptionType(DogType), resolve = _ => None)
      ))

  lazy val FriendlyType: InterfaceType[Any, Any] = InterfaceType(
    "Friendly",
    () =>
      fields(
        Field(
          "bestFriend",
          OptionType(FriendlyType),
          Some("The best friend of this friendly thing"),
          resolve = _ => None)
      ))

  lazy val DogUnionType: ObjectType[Any, Any] = ObjectType(
    "Dog",
    () =>
      fields(
        Field("bestFriend", OptionType(FriendlyUnionType), resolve = _ => None)
      ))

  lazy val HumanUnionType: ObjectType[Any, Any] = ObjectType(
    "Human",
    () =>
      fields(
        Field("bestFriend", OptionType(FriendlyUnionType), resolve = _ => None)
      ))

  lazy val FriendlyUnionType = UnionType("Friendly", types = DogUnionType :: HumanUnionType :: Nil)

  lazy val LazilyInitializedUnionType =
    UnionType("FriendlyButLazy", typesFn = () => DogUnionType :: HumanUnionType :: Nil)

  val CustomScalar = ScalarType[Int](
    "Custom",
    description = Some("Some custom"),
    coerceOutput = (i, _) => ast.IntValue(i),
    coerceUserInput = {
      case i: Int => Right(i)
      case _ => Left(IntCoercionViolation)
    },
    coerceInput = {
      case ast.IntValue(i, _, _) => Right(i)
      case _ => Left(IntCoercionViolation)
    }
  )

  "Type System: build schema from introspection" should {
    "builds a simple schema" in testSchema(
      Schema(
        ObjectType(
          "Simple",
          "This is a simple type",
          fields[Any, Any](
            Field(
              "string",
              OptionType(StringType),
              Some("This is a string field"),
              resolve = _ => "foo")))))

    "builds a simple schema with all operation types" in testSchema(
      Schema(
        query = ObjectType(
          "QueryType",
          "This is a simple query type",
          fields[Any, Any](
            Field(
              "string",
              OptionType(StringType),
              Some("This is a string field"),
              resolve = _ => "foo"))),
        mutation = Some(ObjectType(
          "MutationType",
          "This is a simple mutation type",
          fields[Any, Any](Field(
            "setString",
            OptionType(StringType),
            Some("Set the string field"),
            arguments = Argument[Option[String @@ CoercedScalaResult]](
              "value",
              OptionInputType(StringType)) :: Nil,
            resolve = _ => "foo"
          ))
        )),
        subscription = Some(ObjectType(
          "SubscriptionType",
          "This is a simple subscription type",
          fields[Any, Any](
            Field(
              "string",
              OptionType(StringType),
              Some("This is a string field for sub"),
              resolve = _ => "foo"))
        ))
      ))

    "uses built-in scalars when possible" in {
      val clientSchema =
        testSchema(
          Schema(
            ObjectType(
              "Scalars",
              fields[Any, Any](
                Field("int", IntType, resolve = _ => 1),
                Field("long", LongType, resolve = _ => 1L),
                Field("float", FloatType, resolve = _ => 1.1),
                Field("bool", BooleanType, resolve = _ => true),
                Field("bigInt", BigIntType, resolve = _ => BigInt(1)),
                Field("bigDec", BigDecimalType, resolve = _ => BigDecimal(1.0)),
                Field("id", IDType, resolve = _ => "foo"),
                Field("custom", CustomScalar, resolve = _ => 123),
                Field("string", StringType, resolve = _ => "foo")
              )
            )))

      def fieldType(fieldName: String) =
        clientSchema
          .outputTypes("Scalars")
          .asInstanceOf[ObjectType[_, _]]
          .getField(clientSchema, fieldName)
          .head
          .fieldType

      (fieldType("int") should be).theSameInstanceAs(IntType)
      (fieldType("long") should be).theSameInstanceAs(LongType)
      (fieldType("float") should be).theSameInstanceAs(FloatType)
      (fieldType("bool") should be).theSameInstanceAs(BooleanType)
      (fieldType("bigInt") should be).theSameInstanceAs(BigIntType)
      (fieldType("bigDec") should be).theSameInstanceAs(BigDecimalType)
      (fieldType("id") should be).theSameInstanceAs(IDType)
      (fieldType("string") should be).theSameInstanceAs(StringType)

      (fieldType("custom") shouldNot be).theSameInstanceAs(CustomScalar)
    }

    "builds a schema with a recursive type reference" in testSchema(Schema[Any, Any](RecursiveType))

    "builds a schema with an interface" in {
      val dog = ObjectType(
        "Dog",
        interfaces[Any, Any](FriendlyType),
        fields[Any, Any](
          Field("bestFriend", OptionType(FriendlyType), resolve = _ => None)
        ))

      lazy val human = ObjectType(
        "Human",
        interfaces[Any, Any](FriendlyType),
        fields[Any, Any](
          Field("bestFriend", OptionType(FriendlyType), resolve = _ => None)
        ))

      testSchema(
        Schema(
          query = ObjectType(
            "WithInterface",
            fields[Any, Any](Field("friendly", OptionType(FriendlyType), resolve = _ => None))),
          additionalTypes = dog :: human :: Nil))
    }

    "builds a schema with a union" in testSchema(
      Schema(
        ObjectType(
          "WithUnion",
          fields[Any, Any](Field("friendly", OptionType(FriendlyUnionType), resolve = _ => None)))))

    "builds a schema with complex field values" in testSchema(
      Schema(
        ObjectType(
          "ComplexFields",
          fields[Any, Any](
            Field("string", OptionType(StringType), resolve = _ => None),
            Field(
              "listOfString",
              OptionType(ListType(OptionType(StringType))),
              resolve = _ => None),
            Field("nonNullString", StringType, resolve = _ => "foo"),
            Field("nonNullListOfString", ListType(OptionType(StringType)), resolve = _ => Nil),
            Field("nonNullListOfNonNullString", ListType(StringType), resolve = _ => Nil)
          )
        )))

    "builds a schema with field arguments" in testSchema(
      Schema(
        ObjectType(
          "ArgFields",
          fields[Any, Any](
            Field(
              "one",
              OptionType(StringType),
              Some("A field with a single arg"),
              arguments = Argument[Option[Int @@ CoercedScalaResult]](
                "intArg",
                OptionInputType(IntType),
                description = "This is an int arg") :: Nil,
              resolve = _ => None
            ),
            Field(
              "two",
              OptionType(StringType),
              Some("A field with a two args"),
              arguments = Argument[Option[Seq[Option[Int @@ CoercedScalaResult]]]](
                "listArg",
                OptionInputType(ListInputType(OptionInputType(IntType))),
                description = "This is an list of int arg") ::
                Argument("requiredArg", BooleanType, description = "This is a required arg") ::
                Nil,
              resolve = _ => None
            )
          )
        )))

    "builds a schema with an enum" in {
      val foodType = EnumType(
        "Food",
        Some("Varieties of food stuffs"),
        List(
          EnumValue("VEGETABLES", Some("Foods that are vegetables."), 1),
          EnumValue("FRUITS", Some("Foods that are fruits."), 2),
          EnumValue("OILS", Some("Foods that are oils."), 3),
          EnumValue("DAIRY", Some("Foods that are dairy."), 4),
          EnumValue("MEAT", Some("Foods that are meat."), 5)
        )
      )

      val clientSchema = testSchema(
        Schema(ObjectType(
          "EnumFields",
          fields[Any, Any](Field(
            "food",
            OptionType(foodType),
            Some("Repeats the arg you give it"),
            arguments = Argument[Option[Int @@ CoercedScalaResult]](
              "kind",
              OptionInputType(foodType),
              description = "what kind of food?") :: Nil,
            resolve = _ => None
          ))
        )))

      (clientSchema.allTypes("Food") shouldNot be).theSameInstanceAs(foodType)
      clientSchema.allTypes("Food").asInstanceOf[EnumType[_]].values should be(
        List(
          EnumValue("VEGETABLES", Some("Foods that are vegetables."), "VEGETABLES"),
          EnumValue("FRUITS", Some("Foods that are fruits."), "FRUITS"),
          EnumValue("OILS", Some("Foods that are oils."), "OILS"),
          EnumValue("DAIRY", Some("Foods that are dairy."), "DAIRY"),
          EnumValue("MEAT", Some("Foods that are meat."), "MEAT")
        ))
    }

    "builds a schema with an input object" in {
      val addressType = InputObjectType(
        "Address",
        "An input address",
        List(
          InputField("street", StringType, description = "What street is this address?"),
          InputField("city", StringType, description = "The city the address is within?"),
          InputField(
            "country",
            OptionInputType(StringType),
            "The country (blank will assume USA).",
            "USA")
        )
      )

      testSchema(
        Schema(ObjectType(
          "HasInputObjectFields",
          fields[Any, Any](Field(
            "geocode",
            OptionType(StringType),
            Some("Get a geocode from an address"),
            arguments = Argument(
              "address",
              OptionInputType(addressType),
              description = "The address to lookup") :: Nil,
            resolve = _ => None
          ))
        )))
    }

    "builds a schema with field arguments with default values" in {
      val geoType = InputObjectType(
        "Geo",
        List(
          InputField("lat", OptionInputType(FloatType)),
          InputField("lon", OptionInputType(FloatType))))

      testSchema(Schema(
        ObjectType(
          "ArgFields",
          fields[Any, Any](
            Field(
              "defaultInt",
              OptionType(StringType),
              arguments = Argument[Option[Int @@ CoercedScalaResult], Int](
                "intArg",
                OptionInputType(IntType),
                10) :: Nil,
              resolve = _ => None
            ),
            Field(
              "defaultList",
              OptionType(StringType),
              arguments =
                Argument[Option[Seq[Option[Int @@ CoercedScalaResult]]], Vector[Int] @@ ScalaInput](
                  "listArg",
                  OptionInputType(ListInputType(OptionInputType(IntType))),
                  scalaInput(Vector(1, 2, 3))) :: Nil,
              resolve = _ => None
            ),
            Field(
              "defaultObject",
              OptionType(StringType),
              arguments = Argument(
                "objArg",
                OptionInputType(geoType),
                scalaInput(Map("lat" -> 37.485d, "lon" -> -122.148d))) :: Nil,
              resolve = _ => None
            )
          )
        )))
    }

    "builds a schema with custom directives" in testSchema(
      Schema(
        query = ObjectType(
          "Simple",
          "This is a simple type",
          fields[Any, Any](
            Field(
              "string",
              OptionType(StringType),
              Some("This is a string field"),
              resolve = _ => None))),
        directives = BuiltinDirectives ++ List(
          Directive(
            "customDirective",
            Some("This is a custom directive"),
            shouldInclude = _ => true,
            locations = Set(DirectiveLocation.Field)),
          Directive(
            "customRepeatableDirective",
            Some("This is a custom repeatable directive"),
            shouldInclude = _ => true,
            repeatable = true,
            locations = Set(DirectiveLocation.Field)
          )
        )
      ))

    "builds a schema aware of deprecation" in {
      import sangria.marshalling.sprayJson._

      val serverSchema = testSchema(
        Schema(
          ObjectType(
            "Simple",
            "This is a simple type",
            fields[Any, Any](
              Field(
                "shinyString",
                OptionType(StringType),
                Some("This is a shiny string field"),
                resolve = _ => None),
              Field(
                "deprecatedString",
                OptionType(StringType),
                Some("This is a deprecated string field"),
                deprecationReason = Some("Use shinyString"),
                resolve = _ => None),
              Field(
                "color",
                fieldType = OptionType(EnumType(
                  "Color",
                  values = List(
                    EnumValue("RED", Some("So rosy"), "RED"),
                    EnumValue("GREEN", Some("So grassy"), "GREEN"),
                    EnumValue("BLUE", Some("So calming"), "BLUE"),
                    EnumValue(
                      "MAUVE",
                      Some("So sickening"),
                      "MAUVE",
                      deprecationReason = Some("No longer in fashion"))
                  )
                )),
                resolve = _ => None
              ),
              Field(
                "foo",
                fieldType = OptionType(StringType),
                Some("This is a field with some deprecated args"),
                resolve = _ => None,
                arguments = List(
                  Argument(
                    "arg1",
                    IntType
                  ).withDeprecationReason("use arg2"),
                  Argument("arg2", StringType),
                  Argument(
                    "additionalInput",
                    InputObjectType(
                      "SimpleInput",
                      List(
                        InputField("before", StringType).withDeprecationReason("use after"),
                        InputField("after", IntType)
                      ))
                  )
                )
              )
            )
          ),
          directives = BuiltinDirectives :+ Directive(
            "customDirective",
            arguments = List(
              Argument("deprecated", OptionInputType(IntType)).withDeprecationReason(
                "use notDeprecated"),
              Argument("notDeprecated", OptionInputType(StringType))
            ),
            locations =
              Set(DirectiveLocation.ArgumentDefinition, DirectiveLocation.InputFieldDefinition)
          )
        )
      )

      withClue(serverSchema.renderPretty)(serverSchema.renderPretty.normalizeAllWS should be("""
      schema {
        query: Simple
         }

      enum Color {
        "So rosy"
        RED

        "So grassy"
        GREEN

        "So calming"
        BLUE

        "So sickening"
        MAUVE @deprecated(reason: "No longer in fashion")
      }

      "This is a simple type"
      type Simple {
        "This is a shiny string field"
        shinyString: String

        "This is a deprecated string field"
        deprecatedString: String @deprecated(reason: "Use shinyString")
        color: Color

        "This is a field with some deprecated args"
        foo(arg1: Int! @deprecated(reason: "use arg2"), arg2: String!, additionalInput: SimpleInput!): String
      }

      input SimpleInput {
        before: String! @deprecated(reason: "use after")
        after: Int!
      }

      directive @customDirective(deprecated: Int @deprecated(reason: "use notDeprecated"), notDeprecated: String) on ARGUMENT_DEFINITION | INPUT_FIELD_DEFINITION """.normalizeAllWS))
    }

    "builds a schema with description" in testSchema(
      Schema(
        ObjectType(
          "Simple",
          "This is a simple type",
          fields[Any, Any](Field("shinyString", OptionType(StringType), resolve = _ => None))),
        description = Some("test")))

    "cannot use client schema for general execution" in {
      val clientSchema = testSchema(
        Schema(ObjectType(
          "Query",
          fields[Any, Any](Field(
            "foo",
            OptionType(StringType),
            arguments = Argument[Option[Int @@ CoercedScalaResult]](
              "custom1",
              OptionInputType(CustomScalar)) ::
              Argument[Option[Int @@ CoercedScalaResult]](
                "custom2",
                OptionInputType(CustomScalar)) ::
              Nil,
            resolve = _ => None
          ))
        )))

      checkContainsErrors(
        clientSchema,
        (),
        "query NoNo($v: Custom) { foo(custom1: 123, custom2: $v) }",
        None,
        List(
          """Schema was materialized and cannot be used for any queries except introspection queries.""" -> List(
            Pos(1, 39))),
        args = """{"v": 456}""".parseJson
      )
    }

    "can use client schema for general execution with custom materializer logic" in {
      import sangria.marshalling.sprayJson._

      val serverSchema = Schema(
        ObjectType(
          "Query",
          fields[Any, Any](Field(
            "foo",
            OptionType(IntType),
            arguments = Argument[Option[Int @@ CoercedScalaResult]](
              "custom1",
              OptionInputType(CustomScalar)) ::
              Argument[Option[Int @@ CoercedScalaResult]](
                "custom2",
                OptionInputType(CustomScalar)) ::
              Nil,
            resolve = _ => None
          ))
        ))

      val initialIntrospection = Executor.execute(serverSchema, introspectionQuery).await

      val customBuilder = new DefaultIntrospectionSchemaBuilder[Unit] {
        override def resolveField(
            typeDefinition: IntrospectionType,
            definition: IntrospectionField) =
          ctx =>
            (ctx.parentType.name, ctx.field.name) match {
              case ("Query", "foo") =>
                for {
                  a <- ctx.argOpt[Int]("custom1")
                  b <- ctx.argOpt[Int]("custom2")
                } yield a + b
              case _ => throw DefaultIntrospectionSchemaBuilder.MaterializedSchemaException
            }

        override def scalarCoerceUserInput(definition: IntrospectionScalarType) =
          value =>
            definition.name match {
              case "Custom" =>
                value match {
                  case i: Int => Right(i)
                  case i: BigInt => Right(i.intValue)
                  case _ => Left(IntCoercionViolation)
                }
              case _ => Left(DefaultIntrospectionSchemaBuilder.MaterializedSchemaViolation)
            }

        override def scalarCoerceInput(definition: IntrospectionScalarType) =
          value =>
            definition.name match {
              case "Custom" =>
                value match {
                  case ast.IntValue(i, _, _) => Right(i)
                  case ast.BigIntValue(i, _, _) => Right(i.intValue)
                  case _ => Left(IntCoercionViolation)
                }
              case _ => Left(DefaultIntrospectionSchemaBuilder.MaterializedSchemaViolation)
            }

        override def scalarCoerceOutput(definition: IntrospectionScalarType) =
          (coerced, _) =>
            definition.name match {
              case "Custom" => ast.IntValue(coerced.asInstanceOf[Int])
              case _ => throw DefaultIntrospectionSchemaBuilder.MaterializedSchemaException
            }
      }

      val clientSchema =
        IntrospectionSchemaMaterializer.buildSchema(initialIntrospection, customBuilder)

      check(
        clientSchema,
        (),
        "query Yeah($v: Custom) { foo(custom1: 123, custom2: $v) }",
        Map("data" -> Map("foo" -> 579)),
        """{"v": 456}""".parseJson)
    }
  }
}
