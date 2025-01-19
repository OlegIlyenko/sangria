package sangria.renderer

import sangria.ast
import sangria.execution.Executor
import sangria.marshalling.InputUnmarshaller
import sangria.schema._
import sangria.macros._
import sangria.util.{FutureResultSupport, StringMatchers}
import sangria.introspection.introspectionQuery
import sangria.validation.IntCoercionViolation

import scala.concurrent.ExecutionContext.Implicits.global
import sangria.marshalling.sprayJson._
import sangria.marshalling.ScalaInput.scalaInput
import sangria.marshalling.ScalaInput
import sangria.parser.QueryParser
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import sangria.util.tag.@@ // Scala 3 issue workaround
import sangria.marshalling.FromInput.CoercedScalaResult

class SchemaRenderSpec
    extends AnyWordSpec
    with Matchers
    with FutureResultSupport
    with StringMatchers {
  private[this] def renderForTest[T: InputUnmarshaller](res: T, schema: Schema[_, _]) =
    "\n" + SchemaRenderer.renderSchema(res) + "\n"

  private[this] def renderForTest(schema: Schema[Unit, Unit]) =
    "\n" + SchemaRenderer.renderSchema(schema) + "\n"

  private[this] def renderSingleFieldSchema(tpe: OutputType[_], args: List[Argument[_]] = Nil)(
      implicit render: Schema[Unit, Unit] => String) = {
    val root = ObjectType(
      "Root",
      fields[Unit, Unit](
        Field(
          "singleField",
          tpe.asInstanceOf[OutputType[Unit]],
          arguments = args,
          resolve = _ => ())
      ))
    val schema = Schema(root)

    render(schema)
  }

  val quotes = "\"\"\""

  def `default schema renderer`(implicit render: Schema[Unit, Unit] => String): Unit = {
    "Prints String Field" in {
      renderSingleFieldSchema(OptionType(StringType)) should equal("""
        |schema {
        |  query: Root
        |}
        |
        |type Root {
        |  singleField: String
        |}
        |""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "Prints [String] Field" in {
      renderSingleFieldSchema(OptionType(ListType(OptionType(StringType)))) should equal("""
        |schema {
        |  query: Root
        |}
        |
        |type Root {
        |  singleField: [String]
        |}
        |""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "Prints String! Field" in {
      renderSingleFieldSchema(StringType) should equal("""
        |schema {
        |  query: Root
        |}
        |
        |type Root {
        |  singleField: String!
        |}
        |""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "Prints [String]! Field" in {
      renderSingleFieldSchema(ListType(OptionType(StringType))) should equal("""
        |schema {
        |  query: Root
        |}
        |
        |type Root {
        |  singleField: [String]!
        |}
        |""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "Prints [String!] Field" in {
      renderSingleFieldSchema(OptionType(ListType(StringType))) should equal("""
        |schema {
        |  query: Root
        |}
        |
        |type Root {
        |  singleField: [String!]
        |}
        |""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "Print Object Field" in {
      val foo = ObjectType(
        "Foo",
        fields[Unit, Unit](
          Field("str", OptionType(StringType), resolve = _ => "foo")
        ))

      val root = ObjectType(
        "Root",
        fields[Unit, Unit](
          Field("foo", OptionType(foo), resolve = _ => ())
        ))

      val schema = Schema(root)

      render(schema) should equal("""
        |schema {
        |  query: Root
        |}
        |
        |type Foo {
        |  str: String
        |}
        |
        |type Root {
        |  foo: Foo
        |}
        |""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "Prints String Field With Int Arg" in {
      renderSingleFieldSchema(
        tpe = OptionType(StringType),
        args =
          Argument[Option[Int @@ CoercedScalaResult]]("argOne", OptionInputType(IntType)) :: Nil
      ) should equal("""
        |schema {
        |  query: Root
        |}
        |
        |type Root {
        |  singleField(argOne: Int): String
        |}
        |""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "Prints String Field With Int Arg With Default" in {
      renderSingleFieldSchema(
        tpe = OptionType(StringType),
        args = Argument[Option[Int @@ CoercedScalaResult], Int](
          "argOne",
          OptionInputType(IntType),
          2) :: Nil
      ) should equal("""
        |schema {
        |  query: Root
        |}
        |
        |type Root {
        |  singleField(argOne: Int = 2): String
        |}
        |""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "Prints String Field With Int! Arg" in {
      renderSingleFieldSchema(
        tpe = OptionType(StringType),
        args = Argument("argOne", IntType) :: Nil
      ) should equal("""
        |schema {
        |  query: Root
        |}
        |
        |type Root {
        |  singleField(argOne: Int!): String
        |}
        |""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "Prints String Field With Multiple Args" in {
      renderSingleFieldSchema(
        tpe = OptionType(StringType),
        args = Argument[Option[Int @@ CoercedScalaResult]](
          "argOne",
          OptionInputType(IntType)) :: Argument[Option[String @@ CoercedScalaResult]](
          "argTwo",
          OptionInputType(StringType)) :: Nil
      ) should equal("""
        |schema {
        |  query: Root
        |}
        |
        |type Root {
        |  singleField(argOne: Int, argTwo: String): String
        |}
        |""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "Prints String Field With Multiple Args, First is Default" in {
      renderSingleFieldSchema(
        tpe = OptionType(StringType),
        args =
          Argument[Option[Int @@ CoercedScalaResult], Int]("argOne", OptionInputType(IntType), 1) ::
            Argument[Option[String @@ CoercedScalaResult]]("argTwo", OptionInputType(StringType)) ::
            Argument[Option[Boolean @@ CoercedScalaResult]](
              "argThree",
              OptionInputType(BooleanType)) ::
            Nil
      ) should equal("""
        |schema {
        |  query: Root
        |}
        |
        |type Root {
        |  singleField(argOne: Int = 1, argTwo: String, argThree: Boolean): String
        |}
        |""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "Prints String Field With Multiple Args, Second is Default" in {
      renderSingleFieldSchema(
        tpe = OptionType(StringType),
        args = Argument[Option[Int @@ CoercedScalaResult]]("argOne", OptionInputType(IntType)) ::
          Argument[Option[String @@ CoercedScalaResult], String](
            "argTwo",
            OptionInputType(StringType),
            defaultValue = "foo") ::
          Argument[Option[Boolean @@ CoercedScalaResult]](
            "argThree",
            OptionInputType(BooleanType)) ::
          Nil
      ) should equal("""
        |schema {
        |  query: Root
        |}
        |
        |type Root {
        |  singleField(argOne: Int, argTwo: String = "foo", argThree: Boolean): String
        |}
        |""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "Prints String Field With Multiple Args, Last is Default" in {
      renderSingleFieldSchema(
        tpe = OptionType(StringType),
        args = Argument[Option[Int @@ CoercedScalaResult]]("argOne", OptionInputType(IntType)) ::
          Argument[Option[String @@ CoercedScalaResult]]("argTwo", OptionInputType(StringType)) ::
          Argument[Option[Boolean @@ CoercedScalaResult], Boolean](
            "argThree",
            OptionInputType(BooleanType),
            false) ::
          Nil
      ) should equal("""
        |schema {
        |  query: Root
        |}
        |
        |type Root {
        |  singleField(argOne: Int, argTwo: String, argThree: Boolean = false): String
        |}
        |""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "Print Interface" in {
      val foo = InterfaceType(
        "Foo",
        "My\ndescription",
        fields[Unit, Unit](
          Field(
            "str",
            OptionType(StringType),
            description = Some("field\ndescription"),
            resolve = _ => "foo")
        ))

      val bar = ObjectType(
        "Bar",
        interfaces[Unit, Unit](foo),
        fields[Unit, Unit](Field("str", OptionType(StringType), resolve = _ => "foo")))

      val root = ObjectType(
        "Root",
        fields[Unit, Unit](
          Field("bar", OptionType(bar), resolve = _ => ())
        ))

      val schema = Schema(root)

      render(schema) should equal(s"""
        |schema {
        |  query: Root
        |}
        |
        |type Bar implements Foo {
        |  str: String
        |}
        |
        |$quotes
        |My
        |description
        |$quotes
        |interface Foo {
        |  $quotes
        |  field
        |  description
        |  $quotes
        |  str: String
        |}
        |
        |type Root {
        |  bar: Bar
        |}
        |""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "Print description with some empty lines" in {
      val description =
        """first line followed by empty line
          |
          |second line""".stripMargin
      val foo = InterfaceType(
        "Foo",
        description,
        fields[Unit, Unit](
          Field(
            "str",
            OptionType(StringType),
            description = Some(description),
            resolve = _ => "foo")
        ))

      val bar = ObjectType(
        "Bar",
        description,
        interfaces[Unit, Unit](foo),
        fields[Unit, Unit](
          Field(
            "str",
            OptionType(StringType),
            description = Some(description),
            resolve = _ => "foo")))

      val root = ObjectType(
        "Root",
        fields[Unit, Unit](
          Field("bar", OptionType(bar), description = Some(description), resolve = _ => ())
        ))

      val schema = Schema(root)

      render(schema) should equal(s"""
        |schema {
        |  query: Root
        |}
        |
        |$quotes
        |first line followed by empty line
        |
        |second line
        |$quotes
        |type Bar implements Foo {
        |  $quotes
        |  first line followed by empty line
        |
        |  second line
        |  $quotes
        |  str: String
        |}
        |
        |$quotes
        |first line followed by empty line
        |
        |second line
        |$quotes
        |interface Foo {
        |  $quotes
        |  first line followed by empty line
        |
        |  second line
        |  $quotes
        |  str: String
        |}
        |
        |type Root {
        |  $quotes
        |  first line followed by empty line
        |
        |  second line
        |  $quotes
        |  bar: Bar
        |}
        |""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "Print Multiple Interface" in {
      val foo = InterfaceType(
        "Foo",
        fields[Unit, Unit](
          Field("str", OptionType(StringType), resolve = _ => "foo")
        ))

      val baz = InterfaceType(
        "Baaz",
        fields[Unit, Unit](
          Field("int", OptionType(IntType), resolve = _ => 1)
        ))

      val bar = ObjectType("Bar", interfaces[Unit, Unit](foo, baz), Nil)

      val root = ObjectType(
        "Root",
        fields[Unit, Unit](
          Field("bar", OptionType(bar), resolve = _ => ())
        ))

      val schema = Schema(root)

      render(schema) should equal("""
        |schema {
        |  query: Root
        |}
        |
        |interface Baaz {
        |  int: Int
        |}
        |
        |type Bar implements Foo & Baaz {
        |  str: String
        |  int: Int
        |}
        |
        |interface Foo {
        |  str: String
        |}
        |
        |type Root {
        |  bar: Bar
        |}
        |""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "Print Multiple Interface (with interface hierarchy)" in {
      val foo = InterfaceType(
        "Foo",
        fields[Unit, Unit](
          Field("str", OptionType(StringType), resolve = _ => "foo")
        ))

      val baz = InterfaceType(
        "Baaz",
        fields[Unit, Unit](
          Field("int", OptionType(IntType), resolve = _ => 1)
        ),
        interfaces[Unit, Unit](foo))

      val bar = ObjectType("Bar", interfaces[Unit, Unit](baz), Nil)

      val root = ObjectType(
        "Root",
        fields[Unit, Unit](
          Field("bar", OptionType(bar), resolve = _ => ())
        ))

      val schema = Schema(root)

      render(schema) should equal("""
        |schema {
        |  query: Root
        |}
        |
        |interface Baaz implements Foo {
        |  int: Int
        |  str: String
        |}
        |
        |type Bar implements Baaz & Foo {
        |  int: Int
        |  str: String
        |}
        |
        |interface Foo {
        |  str: String
        |}
        |
        |type Root {
        |  bar: Bar
        |}
        |""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "Print Multiple Interface (with deeper interface hierarchy)" in {
      val foo = InterfaceType(
        "Foo",
        fields[Unit, Unit](
          Field("str", OptionType(StringType), resolve = _ => "foo")
        ))

      val bar = InterfaceType(
        "Bar",
        fields[Unit, Unit](
          Field("int", OptionType(IntType), resolve = _ => 1)
        ),
        interfaces[Unit, Unit](foo))

      val baz = InterfaceType(
        "Baz",
        fields[Unit, Unit](
          Field("bool", OptionType(BooleanType), resolve = _ => true)
        ),
        interfaces[Unit, Unit](bar))

      val fooBarBaz = ObjectType("FooBarBaz", interfaces[Unit, Unit](baz), Nil)

      val root = ObjectType(
        "Root",
        fields[Unit, Unit](
          Field("fooBarBaz", OptionType(fooBarBaz), resolve = _ => ())
        ))

      val schema = Schema(root)

      render(schema) should equal("""
        |schema {
        |  query: Root
        |}
        |
        |interface Bar implements Foo {
        |  int: Int
        |  str: String
        |}
        |
        |interface Baz implements Bar & Foo {
        |  bool: Boolean
        |  int: Int
        |  str: String
        |}
        |
        |interface Foo {
        |  str: String
        |}
        |
        |type FooBarBaz implements Baz & Bar & Foo {
        |  bool: Boolean
        |  int: Int
        |  str: String
        |}
        |
        |type Root {
        |  fooBarBaz: FooBarBaz
        |}
        |""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "Print Unions" in {
      val foo = ObjectType(
        "Foo",
        fields[Unit, Unit](
          Field("bool", OptionType(BooleanType), resolve = _ => true)
        ))

      val bar = ObjectType(
        "Bar",
        fields[Unit, Unit](
          Field("str", OptionType(StringType), resolve = _ => "f")
        ))

      val singleUnion = UnionType("SingleUnion", types = foo :: Nil)
      val multipleUnion = UnionType("MultipleUnion", types = foo :: bar :: Nil)

      val root = ObjectType(
        "Root",
        fields[Unit, Unit](
          Field("single", OptionType(singleUnion), resolve = _ => ()),
          Field("multiple", OptionType(multipleUnion), resolve = _ => ())
        ))

      val schema = Schema(root)

      render(schema) should equal("""
        |schema {
        |  query: Root
        |}
        |
        |type Bar {
        |  str: String
        |}
        |
        |type Foo {
        |  bool: Boolean
        |}
        |
        |union MultipleUnion = Foo | Bar
        |
        |type Root {
        |  single: SingleUnion
        |  multiple: MultipleUnion
        |}
        |
        |union SingleUnion = Foo
        |""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "Print Input Type" in {
      val articleType = InputObjectType(
        "Article",
        "Blog article",
        List(
          InputField("title", StringType, description = "The most important field"),
          InputField(
            "author",
            OptionInputType(StringType),
            description = "The author of the article",
            defaultValue = "Anonymous"),
          InputField("comments", ListInputType(StringType), description = "comments!")
        )
      )

      val inputType = InputObjectType(
        "InputType",
        "My\ndescription",
        List(
          InputField("int", OptionInputType(IntType), description = "My\nfield\ndescription"),
          InputField(
            "article",
            OptionInputType(articleType),
            description = "has a default!",
            defaultValue = scalaInput(
              Map(
                "title" -> "Hello",
                "auhor" -> "Bob",
                "comments" -> List("first!", "looks good!")))
          )
        )
      )

      val root = ObjectType(
        "Root",
        fields[Unit, Unit](
          Field(
            "str",
            OptionType(StringType),
            arguments = Argument("argOne", OptionInputType(inputType)) :: Nil,
            resolve = _ => None)
        ))

      val schema = Schema(root)

      render(schema) should equal(s"""
        |schema {
        |  query: Root
        |}
        |
        |"Blog article"
        |input Article {
        |  "The most important field"
        |  title: String!
        |
        |  "The author of the article"
        |  author: String = "Anonymous"
        |
        |  "comments!"
        |  comments: [String!]!
        |}
        |
        |$quotes
        |My
        |description
        |$quotes
        |input InputType {
        |  $quotes
        |  My
        |  field
        |  description
        |  $quotes
        |  int: Int
        |
        |  "has a default!"
        |  article: Article = {title: "Hello", author: "Anonymous", comments: ["first!", "looks good!"]}
        |}
        |
        |type Root {
        |  str(argOne: InputType): String
        |}
        |""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "Custom Scalar" in {
      val odd = ScalarType[Int](
        "Odd",
        description = Some("My\ndescription"),
        coerceOutput = valueOutput,
        coerceUserInput = {
          case i: Int if i % 2 != 0 => Right(i)
          case i: BigInt if i.isValidInt && i % 2 != BigInt(0) => Right(i.intValue)
          case _ => Left(IntCoercionViolation)
        },
        coerceInput = {
          case ast.IntValue(i, _, _) if i % 2 != 0 => Right(i)
          case ast.BigIntValue(i, _, _) if i.isValidInt && i % 2 != BigInt(0) => Right(i.intValue)
          case _ => Left(IntCoercionViolation)
        }
      )

      val root = ObjectType(
        "Root",
        fields[Unit, Unit](
          Field("odd", OptionType(odd), resolve = _ => None)
        ))

      val schema = Schema(root)

      render(schema) should equal(s"""
        |schema {
        |  query: Root
        |}
        |
        |$quotes
        |My
        |description
        |$quotes
        |scalar Odd
        |
        |type Root {
        |  odd: Odd
        |}
        |""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "Enum" in {
      val rgb = EnumType[Int](
        "RGB",
        description = Some("My\ndescription"),
        values = List(
          EnumValue("RED", description = Some("My Red\n color"), value = 1),
          EnumValue("GREEN", value = 2, deprecationReason = Some("not cool anymore")),
          EnumValue("BLUE", value = 3, deprecationReason = Some(DefaultDeprecationReason))
        )
      )

      val root = ObjectType(
        "Root",
        fields[Unit, Unit](
          Field("rgb", OptionType(rgb), resolve = _ => None)
        ))

      val schema = Schema(root)

      render(schema) should equal(s"""
        |schema {
        |  query: Root
        |}
        |
        |$quotes
        |My
        |description
        |$quotes
        |enum RGB {
        |  $quotes
        |  My Red
        |   color
        |  $quotes
        |  RED
        |  GREEN @deprecated(reason: "not cool anymore")
        |  BLUE @deprecated
        |}
        |
        |type Root {
        |  rgb: RGB
        |}
        |""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "Directive" in {
      val myDirective = Directive(
        "myDirective",
        description = Some(
          "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec posuere ornare nulla, non bibendum nisi dictum at. Etiam consequat velit ut leo fringilla mollis. Integer ut fringilla ante. Curabitur sagittis malesuada nibh sed vestibulum.\nNunc eu metus felis. Cras tellus nibh, porta nec lorem quis, elementum egestas tellus. Etiam vitae tellus vitae dui varius lobortis."),
        arguments = Argument[Option[Seq[String @@ CoercedScalaResult]], List[String] @@ ScalaInput](
          "first",
          OptionInputType(ListInputType(StringType)),
          "Some descr",
          scalaInput(List("foo", "bar", "baz"))) ::
          Argument[Option[Seq[String @@ CoercedScalaResult]], Int @@ ScalaInput](
            "middle",
            OptionInputType(ListInputType(StringType)),
            "Several\n  lines\nof \"description\"",
            scalaInput(123)) ::
          Argument[Option[Int @@ CoercedScalaResult]](
            "last",
            OptionInputType(IntType),
            "Another descr") ::
          Nil,
        locations = Set(DirectiveLocation.FieldDefinition, DirectiveLocation.InputFieldDefinition),
        shouldInclude = _ => true
      )

      val root = ObjectType(
        "Root",
        fields[Unit, Unit](Field("foo", OptionType(StringType), resolve = _ => None)))

      val schema = Schema(root, directives = BuiltinDirectives :+ myDirective)

      render(schema) should equal(s"""
        |schema {
        |  query: Root
        |}
        |
        |type Root {
        |  foo: String
        |}
        |
        |$quotes
        |Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec posuere ornare nulla, non bibendum nisi dictum at. Etiam consequat velit ut leo fringilla mollis. Integer ut fringilla ante. Curabitur sagittis malesuada nibh sed vestibulum.
        |Nunc eu metus felis. Cras tellus nibh, porta nec lorem quis, elementum egestas tellus. Etiam vitae tellus vitae dui varius lobortis.
        |$quotes
        |directive @myDirective(
        |  "Some descr"
        |  first: [String!] = ["foo", "bar", "baz"],
        |
        |  $quotes
        |  Several
        |    lines
        |  of "description"
        |  $quotes
        |  middle: [String!],
        |
        |  "Another descr"
        |  last: Int) on FIELD_DEFINITION | INPUT_FIELD_DEFINITION
        |""".stripMargin)(after.being(strippedOfCarriageReturns))
    }
  }

  "Introspection-based Schema Renderer" should {
    behave.like(`default schema renderer`(schema =>
      renderForTest(Executor.execute(schema, introspectionQuery).await, schema)))

    "throw an exception if introspection results contain some errors" in {
      val root = ObjectType(
        "Root",
        fields[Unit, Unit](
          Field("singleField", StringType, resolve = _ => "")
        ))

      val schema = Schema(root)

      an[IllegalArgumentException] should be thrownBy
        SchemaRenderer.renderSchema(
          Executor.execute(schema, graphql"{someUnknownField}").awaitAndRecoverQueryAnalysis)
    }
  }

  "Schema-based Schema Renderer" should
    behave.like(`default schema renderer`(schema => renderForTest(schema)))

  "Introspection Schema Renderer" should {
    "Print Introspection Schema" in {
      val schema =
        Schema(ObjectType("Root", fields[Unit, Unit](Field("foo", IntType, resolve = _ => 1))))
      val rendered = SchemaRenderer.renderSchema(
        Executor.execute(schema, introspectionQuery).await,
        SchemaFilter.introspection)

      ("\n" + rendered + "\n") should equal(s"""
        |$quotes
        |A Directive provides a way to describe alternate runtime execution and type validation behavior in a GraphQL document.
        |
        |In some cases, you need to provide options to alter GraphQL’s execution behavior in ways field arguments will not suffice, such as conditionally including or skipping a field. Directives provide this by describing additional information to the executor.
        |$quotes
        |type __Directive {
        |  name: String!
        |  description: String
        |  locations: [__DirectiveLocation!]!
        |  args(includeDeprecated: Boolean = false): [__InputValue!]!
        |
        |  "Permits using the directive multiple times at the same location."
        |  isRepeatable: Boolean!
        |}
        |
        |"A Directive can be adjacent to many parts of the GraphQL language, a __DirectiveLocation describes one such possible adjacencies."
        |enum __DirectiveLocation {
        |  "Location adjacent to a query operation."
        |  QUERY
        |
        |  "Location adjacent to a mutation operation."
        |  MUTATION
        |
        |  "Location adjacent to a subscription operation."
        |  SUBSCRIPTION
        |
        |  "Location adjacent to a field."
        |  FIELD
        |
        |  "Location adjacent to a fragment definition."
        |  FRAGMENT_DEFINITION
        |
        |  "Location adjacent to a fragment spread."
        |  FRAGMENT_SPREAD
        |
        |  "Location adjacent to an inline fragment."
        |  INLINE_FRAGMENT
        |
        |  "Location adjacent to a variable definition."
        |  VARIABLE_DEFINITION
        |
        |  "Location adjacent to a schema definition."
        |  SCHEMA
        |
        |  "Location adjacent to a scalar definition."
        |  SCALAR
        |
        |  "Location adjacent to an object type definition."
        |  OBJECT
        |
        |  "Location adjacent to a field definition."
        |  FIELD_DEFINITION
        |
        |  "Location adjacent to an argument definition."
        |  ARGUMENT_DEFINITION
        |
        |  "Location adjacent to an interface definition."
        |  INTERFACE
        |
        |  "Location adjacent to a union definition."
        |  UNION
        |
        |  "Location adjacent to an enum definition."
        |  ENUM
        |
        |  "Location adjacent to an enum value definition."
        |  ENUM_VALUE
        |
        |  "INPUT_OBJECT"
        |  INPUT_OBJECT
        |
        |  "Location adjacent to an input object field definition."
        |  INPUT_FIELD_DEFINITION
        |}
        |
        |"One possible value for a given Enum. Enum values are unique values, not a placeholder for a string or numeric value. However an Enum value is returned in a JSON response as a string."
        |type __EnumValue {
        |  name: String!
        |  description: String
        |  isDeprecated: Boolean!
        |  deprecationReason: String
        |}
        |
        |"Object and Interface types are described by a list of Fields, each of which has a name, potentially a list of arguments, and a return type."
        |type __Field {
        |  name: String!
        |  description: String
        |  args(includeDeprecated: Boolean = false): [__InputValue!]!
        |  type: __Type!
        |  isDeprecated: Boolean!
        |  deprecationReason: String
        |}
        |
        |"Arguments provided to Fields or Directives and the input fields of an InputObject are represented as Input Values which describe their type and optionally a default value."
        |type __InputValue {
        |  name: String!
        |  description: String
        |  type: __Type!
        |
        |  "A GraphQL-formatted string representing the default value for this input value."
        |  defaultValue: String
        |  isDeprecated: Boolean
        |  deprecationReason: String
        |}
        |
        |"A GraphQL Schema defines the capabilities of a GraphQL server. It exposes all available types and directives on the server, as well as the entry points for query, mutation, and subscription operations."
        |type __Schema {
        |  description: String
        |
        |  "A list of all types supported by this server."
        |  types: [__Type!]!
        |
        |  "The type that query operations will be rooted at."
        |  queryType: __Type!
        |
        |  "If this server supports mutation, the type that mutation operations will be rooted at."
        |  mutationType: __Type
        |
        |  "If this server support subscription, the type that subscription operations will be rooted at."
        |  subscriptionType: __Type
        |
        |  "A list of all directives supported by this server."
        |  directives: [__Directive!]!
        |}
        |
        |$quotes
        |The fundamental unit of any GraphQL Schema is the type. There are many kinds of types in GraphQL as represented by the `__TypeKind` enum.
        |
        |Depending on the kind of a type, certain fields describe information about that type. Scalar types provide no information beyond a name and description, while Enum types provide their values. Object and Interface types provide the fields they describe. Abstract types, Union and Interface, provide the Object types possible at runtime. List and NonNull types compose other types.
        |$quotes
        |type __Type {
        |  kind: __TypeKind!
        |  name: String
        |  description: String
        |  fields(includeDeprecated: Boolean = false): [__Field!]
        |  interfaces: [__Type!]
        |  possibleTypes: [__Type!]
        |  enumValues(includeDeprecated: Boolean = false): [__EnumValue!]
        |  inputFields(includeDeprecated: Boolean = false): [__InputValue!]
        |  ofType: __Type
        |}
        |
        |"An enum describing what kind of type a given `__Type` is."
        |enum __TypeKind {
        |  "Indicates this type is a scalar."
        |  SCALAR
        |
        |  "Indicates this type is an object. `fields` and `interfaces` are valid fields."
        |  OBJECT
        |
        |  "Indicates this type is an interface. `fields` and `possibleTypes` are valid fields."
        |  INTERFACE
        |
        |  "Indicates this type is a union. `possibleTypes` is a valid field."
        |  UNION
        |
        |  "Indicates this type is an enum. `enumValues` is a valid field."
        |  ENUM
        |
        |  "Indicates this type is an input object. `inputFields` is a valid field."
        |  INPUT_OBJECT
        |
        |  "Indicates this type is a list. `ofType` is a valid field."
        |  LIST
        |
        |  "Indicates this type is a non-null. `ofType` is a valid field."
        |  NON_NULL
        |}
        |""".stripMargin)(after.being(strippedOfCarriageReturns))
    }
  }

  "Rendering extensions" should {
    def cycleRender(document: ast.Document) = {
      val pretty = QueryRenderer.renderPretty(document)
      val compact = QueryRenderer.renderCompact(document)

      QueryRenderer.renderPretty(QueryParser.parse(pretty).get) should be(pretty)
      QueryRenderer.renderCompact(QueryParser.parse(compact).get) should be(compact)

      "\n" + pretty + "\n"
    }

    "render object types" in {
      val schema =
        graphql"""
          # just testing
          extend type Obj @someDir(arg: [
            "foo"
          ])

          extend type ObjWithFields@someDir(arg: 1)@anotherDir{
            "some docs"
            foo: Bar @foo
          }

          extend type Obj1 implements Bar&Baz@foo(arg: 1)
        """

      cycleRender(schema) should equal("""
        |# just testing
        |extend type Obj @someDir(arg: ["foo"])
        |
        |extend type ObjWithFields @someDir(arg: 1) @anotherDir {
        |  "some docs"
        |  foo: Bar @foo
        |}
        |
        |extend type Obj1 implements Bar & Baz @foo(arg: 1)
        |""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "render input types" in {
      val schema =
        graphql"""
          # just testing
          extend input Inp@someDir(arg: [
            "foo"
          ])

          extend input InpWithFields@someDir(arg: 1)@anotherDir{
            "some docs"
            foo: Bar={
              hello: "world"
              }
          }
        """

      cycleRender(schema) should equal("""
        |# just testing
        |extend input Inp @someDir(arg: ["foo"])
        |
        |extend input InpWithFields @someDir(arg: 1) @anotherDir {
        |  "some docs"
        |  foo: Bar = {hello: "world"}
        |}
        |""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "render interface types" in {
      val schema =
        graphql"""
          # just testing
          extend interface Foo@someDir(arg: [
            "foo"
          ])

          extend interface FooWithFields @someDir(arg: 1)@anotherDir{
            "some docs"
            foo(arg: Int = 1@hello): Bar @dir(test:true)
          }
        """

      cycleRender(schema) should equal("""
        |# just testing
        |extend interface Foo @someDir(arg: ["foo"])
        |
        |extend interface FooWithFields @someDir(arg: 1) @anotherDir {
        |  "some docs"
        |  foo(arg: Int = 1 @hello): Bar @dir(test: true)
        |}
        |""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "render interface implementing interfaces types" in {
      val schema =
        graphql"""
          # just testing
          extend interface Foo@someDir(arg: [
            "foo"
          ])

          extend interface FooWithFields implements Bar @someDir(arg: 1)@anotherDir{
            "some docs"
            foo(arg: Int = 1@hello): Bar @dir(test:true)
          } 
        """

      cycleRender(schema) should equal("""
        |# just testing
        |extend interface Foo @someDir(arg: ["foo"])
        |
        |extend interface FooWithFields implements Bar @someDir(arg: 1) @anotherDir {
        |  "some docs"
        |  foo(arg: Int = 1 @hello): Bar @dir(test: true)
        |}
        |""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "render union types" in {
      val schema =
        graphql"""
          # just testing
          extend union Foo@someDir(arg: [
            "foo"
          ])

          extend union FooWithStuff @someDir(arg: 1)@anotherDir=Hello|World
        """

      cycleRender(schema) should equal("""
        |# just testing
        |extend union Foo @someDir(arg: ["foo"])
        |
        |extend union FooWithStuff @someDir(arg: 1) @anotherDir = Hello | World
        |""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "render enum types" in {
      val schema =
        graphql"""
          # just testing
          extend enum Foo@someDir(arg: [
            "foo"
          ])

          extend enum FooWithStuff @someDir(arg: 1)@anotherDir{Hello "docs"World}
        """

      cycleRender(schema) should equal("""
        |# just testing
        |extend enum Foo @someDir(arg: ["foo"])
        |
        |extend enum FooWithStuff @someDir(arg: 1) @anotherDir {
        |  Hello
        |
        |  "docs"
        |  World
        |}
        |""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "render schema extensions" in {
      val schema =
        graphql"""
          # comment 0
          extend schema @dir4

          schema @dir1 {
            query: Query
          }

          # comment 1
          extend schema @dir2 @dir3(test: true) {
            mutation: Mutation

            # another comment
          }
        """

      cycleRender(schema) should equal("""
        |# comment 0
        |extend schema @dir4
        |
        |schema @dir1 {
        |  query: Query
        |}
        |
        |# comment 1
        |extend schema @dir2 @dir3(test: true) {
        |  mutation: Mutation
        |
        |  # another comment
        |}
        |""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "render schema description" in {
      val schema =
        graphql"""
          #comment1
          "test"
          #comment 2
          schema @dir1 {
            query: Query
          }
        """

      cycleRender(schema) should equal("""
        |# comment1
        |"test"
        |# comment 2
        |schema @dir1 {
        |  query: Query
        |}
        |""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "render scalar types" in {
      val schema =
        graphql"""
          # just testing
          extend scalar Int@someDir(arg: [
            "foo"
          ])
        """

      cycleRender(schema) should equal("""
        |# just testing
        |extend scalar Int @someDir(arg: ["foo"])
        |""".stripMargin)(after.being(strippedOfCarriageReturns))
    }
  }
}
