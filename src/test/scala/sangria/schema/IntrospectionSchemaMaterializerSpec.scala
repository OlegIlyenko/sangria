package sangria.schema

import org.scalatest.{Matchers, WordSpec}
import sangria.ast
import sangria.execution.Executor
import sangria.introspection.introspectionQuery
import sangria.marshalling.MarshallerCapability
import sangria.marshalling.ScalaInput.scalaInput
import sangria.util.SimpleGraphQlSupport.{check, checkContainsErrors}
import sangria.util.{FutureResultSupport, Pos}
import sangria.validation.IntCoercionViolation
import spray.json._

import scala.concurrent.ExecutionContext.Implicits.global

class IntrospectionSchemaMaterializerSpec extends WordSpec with Matchers with FutureResultSupport {

  // Test property:
  // Given a server's schema, a client may query that server with introspection,
  // and use the result to produce a client-side representation of the schema
  // by using "buildClientSchema". If the client then runs the introspection
  // query against the client-side schema, it should get a result identical to
  // what was returned by the server.
  def testSchema(serverSchema: Schema[Unit, Unit]): Schema[Unit, Unit] = {
    import sangria.marshalling.queryAst._

    val initialIntrospection = Executor.execute(serverSchema, introspectionQuery).await
    val clientSchema = IntrospectionSchemaMaterializer.buildSchema(initialIntrospection, MaterializationLogic.withDefaultValues[Unit, ast.Value])
    val secondIntrospection = Executor.execute(clientSchema, introspectionQuery).await

    initialIntrospection should be (secondIntrospection)

    clientSchema
  }

  lazy val RecursiveType: ObjectType[Unit, Unit] = ObjectType("Recur", () ⇒ fields(
    Field("recur", OptionType(RecursiveType), resolve = _ ⇒ None)
  ))

  lazy val DogType: ObjectType[Unit, Unit] = ObjectType("Dog", fieldsFn = () ⇒ fields(
    Field("bestFriend", OptionType(HumanType), resolve = _ ⇒ None),
    Field("name", OptionType(StringType), resolve = _ ⇒ None)
  ), interfaces = FriendlyType :: Nil)

  lazy val HumanType: ObjectType[Unit, Unit] = ObjectType("Human", fieldsFn = () ⇒ fields(
    Field("bestFriend", OptionType(DogType), resolve = _ ⇒ None),
    Field("firstName", OptionType(StringType), resolve = _ ⇒ None)
  ), interfaces = FriendlyType :: Nil)

  lazy val FriendlyType: InterfaceType[Unit, Unit] = InterfaceType("FriendlyInterface", () ⇒ fields(
    Field("bestFriend", OptionType(FriendlyType), Some("The best friend of this friendly thing"), resolve = _ ⇒ None)
  ))

  lazy val DogUnionType: ObjectType[Unit, Unit] = ObjectType("DogUnion", () ⇒ fields(
    Field("bestFriend", OptionType(FriendlyUnionType), resolve = _ ⇒ None),
    Field("name", OptionType(StringType), resolve = _ ⇒ None)
  ))

  lazy val HumanUnionType: ObjectType[Unit, Unit] = ObjectType("HumanUnion", () ⇒ fields(
    Field("bestFriend", OptionType(FriendlyUnionType), resolve = _ ⇒ None),
    Field("firstName", OptionType(StringType), resolve = _ ⇒ None)
  ))

  lazy val FriendlyUnionType = UnionType("FriendlyUnion", types = DogUnionType :: HumanUnionType :: Nil)

  val CustomScalar = ScalarType[Int]("Custom",
    description = Some("Some custom"),
    coerceOutput = (i, _) ⇒ ast.IntValue(i),
    coerceUserInput = {
      case i: Int ⇒ Right(i)
      case _ ⇒ Left(IntCoercionViolation)
    },
    coerceInput = {
      case ast.IntValue(i, _, _) ⇒ Right(i)
      case _ ⇒ Left(IntCoercionViolation)
    })

  "Type System: build schema from introspection" should {
    "builds a simple schema" in testSchema(
      Schema(ObjectType("Simple", "This is a simple type", fields[Unit, Unit](
        Field("string", OptionType(StringType), Some("This is a string field"), resolve = _ ⇒ "foo")))))

    "builds a simple schema with all operation types" in testSchema(
      Schema(
        query =
          ObjectType("QueryType", "This is a simple query type", fields[Unit, Unit](
            Field("string", OptionType(StringType), Some("This is a string field"), resolve = _ ⇒ "foo"))),
        mutation = Some(
          ObjectType("MutationType", "This is a simple mutation type", fields[Unit, Unit](
            Field("setString", OptionType(StringType), Some("Set the string field"),
              arguments = Argument("value", OptionInputType(StringType)) :: Nil,
              resolve = _ ⇒ "foo")))),
        subscription = Some(
          ObjectType("SubscriptionType", "This is a simple subscription type", fields[Unit, Unit](
            Field("string", OptionType(StringType), Some("This is a string field for sub"), resolve = _ ⇒ "foo"))))
        ))

    "uses built-in scalars when possible" in {
      val clientSchema = testSchema(
        Schema(ObjectType("Scalars", fields[Unit, Unit](
          Field("int", IntType, resolve = _ ⇒ 1),
          Field("long", LongType, resolve = _ ⇒ 1L),
          Field("float", FloatType, resolve = _ ⇒ 1.1),
          Field("bool", BooleanType, resolve = _ ⇒ true),
          Field("bigInt", BigIntType, resolve = _ ⇒ BigInt(1)),
          Field("bigDec", BigDecimalType, resolve = _ ⇒ BigDecimal(1.0)),
          Field("id", IDType, resolve = _ ⇒ "foo"),
          Field("custom", CustomScalar, resolve = _ ⇒ 123),
          Field("string", StringType, resolve = _ ⇒ "foo")))))

      def fieldType(fieldName: String) =
        clientSchema.outputTypes("Scalars").asInstanceOf[ObjectType[_, _]].getField(clientSchema, fieldName).head.fieldType

      fieldType("int") should be theSameInstanceAs IntType
      fieldType("long") should be theSameInstanceAs LongType
      fieldType("float") should be theSameInstanceAs FloatType
      fieldType("bool") should be theSameInstanceAs BooleanType
      fieldType("bigInt") should be theSameInstanceAs BigIntType
      fieldType("bigDec") should be theSameInstanceAs BigDecimalType
      fieldType("id") should be theSameInstanceAs IDType
      fieldType("string") should be theSameInstanceAs StringType

      fieldType("custom") shouldNot be theSameInstanceAs CustomScalar
    }

    "builds a schema with a recursive type reference" in testSchema(Schema(RecursiveType))

    "builds a schema with an interface" in {
      val dog = ObjectType("Dog", interfaces[Unit, Unit](FriendlyType), fields[Unit, Unit](
        Field("bestFriend", OptionType(FriendlyType), resolve = _ ⇒ None)
      ))

      lazy val human = ObjectType("Human", interfaces[Unit, Unit](FriendlyType), fields[Unit, Unit](
        Field("bestFriend", OptionType(FriendlyType), resolve = _ ⇒ None)
      ))

      testSchema(Schema(
        query = ObjectType("WithInterface", fields[Unit, Unit](
          Field("friendly", OptionType(FriendlyType), resolve = _ ⇒ None))),
        additionalTypes = dog :: human :: Nil))
    }

    "builds a schema with a union" in testSchema(
      Schema(ObjectType("WithUnion", fields[Unit, Unit](
        Field("friendly", OptionType(FriendlyUnionType), resolve = _ ⇒ None)))))

    "builds a schema with complex field values" in testSchema(
      Schema(ObjectType("ComplexFields", fields[Unit, Unit](
        Field("string", OptionType(StringType), resolve = _ ⇒ None),
        Field("listOfString", OptionType(ListType(OptionType(StringType))), resolve = _ ⇒ None),
        Field("nonNullString", StringType, resolve = _ ⇒ "foo"),
        Field("nonNullListOfString", ListType(OptionType(StringType)), resolve = _ ⇒ Nil),
        Field("nonNullListOfNonNullString", ListType(StringType), resolve = _ ⇒ Nil)))))

    "builds a schema with field arguments" in testSchema(
      Schema(ObjectType("ArgFields", fields[Unit, Unit](
        Field("one", OptionType(StringType), Some("A field with a single arg"),
          arguments = Argument("intArg", OptionInputType(IntType), description = "This is an int arg") :: Nil,
          resolve = _ ⇒ None),
        Field("two", OptionType(StringType), Some("A field with a two args"),
          arguments =
            Argument("listArg", OptionInputType(ListInputType(OptionInputType(IntType))), description = "This is an list of int arg") ::
            Argument("requiredArg", BooleanType, description = "This is a required arg") ::
            Nil,
          resolve = _ ⇒ None)))))

    "builds a schema with an enum" in {
      val foodType = EnumType("Food", Some("Varieties of food stuffs"), List(
        EnumValue("VEGETABLES", Some("Foods that are vegetables."), 1),
        EnumValue("FRUITS", Some("Foods that are fruits."), 2),
        EnumValue("OILS", Some("Foods that are oils."), 3),
        EnumValue("DAIRY", Some("Foods that are dairy."), 4),
        EnumValue("MEAT", Some("Foods that are meat."), 5)))

      val clientSchema = testSchema(
        Schema(ObjectType("EnumFields", fields[Unit, Unit](
          Field("food", OptionType(foodType), Some("Repeats the arg you give it"),
            arguments = Argument("kind", OptionInputType(foodType), description = "what kind of food?") :: Nil,
            resolve = _ ⇒ None)))))


      clientSchema.allTypes("Food") shouldNot be theSameInstanceAs foodType
      clientSchema.allTypes("Food").asInstanceOf[EnumType[_]].values should be (List(
        EnumValue("VEGETABLES", Some("Foods that are vegetables."), "VEGETABLES"),
        EnumValue("FRUITS", Some("Foods that are fruits."), "FRUITS"),
        EnumValue("OILS", Some("Foods that are oils."), "OILS"),
        EnumValue("DAIRY", Some("Foods that are dairy."), "DAIRY"),
        EnumValue("MEAT", Some("Foods that are meat."), "MEAT")))
    }

    "builds a schema with an input object" in {
      val addressType = InputObjectType("Address", "An input address", List(
        InputField("street", StringType, description = "What street is this address?"),
        InputField("city", StringType, description = "The city the address is within?"),
        InputField("country", OptionInputType(StringType), "The country (blank will assume USA).", "USA")))

      testSchema(
        Schema(ObjectType("HasInputObjectFields", fields[Unit, Unit](
          Field("geocode", OptionType(StringType), Some("Get a geocode from an address"),
            arguments = Argument("address", OptionInputType(addressType), description = "The address to lookup") :: Nil,
            resolve = _ ⇒ None)))))
    }

    "builds a schema with field arguments with default values" in {
      val geoType = InputObjectType("Geo", List(
        InputField("lat", OptionInputType(FloatType)),
        InputField("lon", OptionInputType(FloatType))))

      testSchema(
        Schema(ObjectType("ArgFields", fields[Unit, Unit](
          Field("defaultInt", OptionType(StringType),
            arguments = Argument("intArg", OptionInputType(IntType), 10) :: Nil,
            resolve = _ ⇒ None),
          Field("defaultList", OptionType(StringType),
            arguments = Argument("listArg", OptionInputType(ListInputType(OptionInputType(IntType))), scalaInput(Vector(1, 2, 3))) :: Nil,
            resolve = _ ⇒ None),
          Field("defaultObject", OptionType(StringType),
            arguments = Argument("objArg", OptionInputType(geoType), scalaInput(Map("lat" → 37.485D, "lon" → -122.148D))) :: Nil,
            resolve = _ ⇒ None)))))
    }

    "builds a schema with custom directives" in testSchema(
      Schema(
        query = ObjectType("Simple", "This is a simple type", fields[Unit, Unit](
          Field("string", OptionType(StringType), Some("This is a string field"),
            resolve = _ ⇒ None))),
        directives = List(Directive("customDirective", Some("This is a custom directive"),
          shouldInclude = _ ⇒ true,
          locations = Set(DirectiveLocation.Field)))))

    "builds a schema aware of deprecation" in testSchema(
      Schema(ObjectType("Simple", "This is a simple type", fields[Unit, Unit](
        Field("shinyString", OptionType(StringType), Some("This is a shiny string field"),
          resolve = _ ⇒ None),
        Field("deprecatedString", OptionType(StringType), Some("This is a deprecated string field"),
          deprecationReason = Some("Use shinyString"),
          resolve = _ ⇒ None),
        Field("color",
          fieldType = OptionType(EnumType("Color", values = List(
            EnumValue("RED", Some("So rosy"), "RED"),
            EnumValue("GREEN", Some("So grassy"), "GREEN"),
            EnumValue("BLUE", Some("So calming"), "BLUE"),
            EnumValue("MAUVE", Some("So sickening"), "MAUVE", deprecationReason = Some("No longer in fashion"))))),
          resolve = _ ⇒ None)))))

    "cannot use client schema for general execution" in {
      val clientSchema = testSchema(
        Schema(ObjectType("Query", fields[Unit, Unit](
          Field("foo", OptionType(StringType),
            arguments =
              Argument("custom1", OptionInputType(CustomScalar)) ::
              Argument("custom2", OptionInputType(CustomScalar)) ::
              Nil,
            resolve = _ ⇒ None)))))

      checkContainsErrors(clientSchema, (),
        "query NoNo($v: Custom) { foo(custom1: 123, custom2: $v) }",
        null,
        List("""Schema was materialized and cannot be used for any queries except introspection queries.""" → List(Pos(1, 39))),
        args = """{"v": 456}""".parseJson
      )
    }

    "can use client schema for general execution with custom materializer logic" in {
      import sangria.marshalling.sprayJson._

      case class Dog(name: String, bestFriend: Option[Human])
      case class Human(firstName: String, bestFriend: Option[Dog])

      lazy val dog: Dog = Dog("spot", None)
      lazy val human: Human = Human("bob", None)

      val serverSchema = Schema(ObjectType("Query", fields[Unit, Unit](
        Field("foo", OptionType(IntType),
          arguments =
            Argument("custom1", OptionInputType(CustomScalar)) ::
            Argument("custom2", OptionInputType(CustomScalar)) ::
            Nil,
          resolve = _ ⇒ None),
        Field("friendlyInterface", OptionType(FriendlyType), resolve = _ ⇒ None),
        Field("friendlyUnion", OptionType(FriendlyUnionType), resolve = _ ⇒ None)
      )), additionalTypes = DogType :: HumanType :: Nil)

      val initialIntrospection = Executor.execute(serverSchema, introspectionQuery).await

      val customLogic = new DefaultMaterializationLogic[Unit] {
        override def resolveField(ctx: Context[Unit, _]) = (ctx.parentType.name, ctx.field.name, ctx.value) match {
          case ("Query", "foo", _) ⇒
            for {
              a ← ctx.argOpt[Int]("custom1")
              b ← ctx.argOpt[Int]("custom2")
            } yield a + b
          case ("Query", "friendlyInterface", _)                    ⇒ dog
          case ("Query", "friendlyUnion", _)                        ⇒ human
          case ("Dog" | "DogUnion", "name", dog: Dog)               ⇒ dog.name
          case ("Human" | "HumanUnion", "firstName", human: Human)  ⇒ human.firstName
          case _                                                    ⇒ super.resolveField(ctx)
        }

        override def coerceScalarUserInput(scalarName: String, value: Any) = scalarName match {
          case "Custom" ⇒ value match {
            case i: Int ⇒ Right(i)
            case i: BigInt ⇒ Right(i.intValue)
            case _ ⇒ Left(IntCoercionViolation)
          }
          case _ ⇒ super.coerceScalarUserInput(scalarName, value)
        }

        override def coerceScalarOutput(scalarName: String, coerced: Any, caps: Set[MarshallerCapability]) = scalarName match {
          case "Custom" ⇒ ast.IntValue(coerced.asInstanceOf[Int])
          case _ ⇒ super.coerceScalarOutput(scalarName, coerced, caps)
        }

        override def coerceScalarInput(scalarName: String, value: ast.Value) = scalarName match {
          case "Custom" ⇒ value match {
            case ast.IntValue(i, _, _) ⇒ Right(i)
            case ast.BigIntValue(i, _, _) ⇒ Right(i.intValue)
            case _ ⇒ Left(IntCoercionViolation)
          }
          case _ ⇒ super.coerceScalarInput(scalarName, value)
        }

        override def classForType(name: String): Option[Class[_]] = name match {
          case "Dog" | "DogUnion"     ⇒ Some(classOf[Dog])
          case "Human" | "HumanUnion" ⇒ Some(classOf[Human])
          case _                      ⇒ None
        }

      }

      val clientSchema = IntrospectionSchemaMaterializer.buildSchema(initialIntrospection, customLogic)

      check(clientSchema, (),
        "query Yeah($v: Custom) { " +
          "foo(custom1: 123, custom2: $v) " +
          "friendlyInterface { ...dogFields ...humanFields } " +
          "friendlyUnion { ...dogUnionFields ...humanUnionFields } " +
          "} " +
          "fragment dogFields on Dog { name } " +
          "fragment humanFields on Human { firstName }" +
          "fragment dogUnionFields on DogUnion { name } " +
          "fragment humanUnionFields on HumanUnion { firstName }",
        Map("data" → Map(
          "foo" → 579,
          "friendlyInterface" → Map("name" → "spot"),
          "friendlyUnion" → Map("firstName" → "bob")
        )),
        """{"v": 456}""".parseJson
      )
    }
  }
}
