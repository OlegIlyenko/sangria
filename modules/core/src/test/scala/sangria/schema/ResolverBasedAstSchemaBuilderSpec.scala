package sangria.schema

import java.util.UUID
import java.util.concurrent.atomic.AtomicInteger
import sangria.ast
import sangria.execution.{Executor, QueryReducer, UserFacingError}
import sangria.macros._
import sangria.marshalling.InputUnmarshaller
import sangria.schema.AstSchemaBuilder.{FieldName, TypeName, resolverBased}
import sangria.schema.{DirectiveLocation => DL}
import sangria.util.{FutureResultSupport, Pos}
import sangria.validation.{BaseViolation, DocumentAnalyzer}
import spray.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}
import sangria.util.SimpleGraphQlSupport._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import sangria.util.tag.@@ // Scala 3 issue workaround
import sangria.marshalling.FromInput.CoercedScalaResult

class ResolverBasedAstSchemaBuilderSpec extends AnyWordSpec with Matchers with FutureResultSupport {
  case object UUIDViolation extends BaseViolation("Invalid UUID")

  private[this] def parseUuid(s: String) = Try(UUID.fromString(s)) match {
    case Success(s) => Right(s)
    case Failure(_) => Left(UUIDViolation)
  }

  private[this] val UUIDType =
    ScalarType[UUID](
      "UUID",
      coerceOutput = (v, _) => v.toString,
      coerceUserInput = {
        case s: String => parseUuid(s)
        case _ => Left(UUIDViolation)
      },
      coerceInput = {
        case ast.StringValue(s, _, _, _, _) => parseUuid(s)
        case _ => Left(UUIDViolation)
      }
    )

  "ResolverBasedAstSchemaBuilder" should {
    "provide basic resolution capabilities" in {
      import sangria.marshalling.sprayJson._

      val ValueArg: Argument[String] = Argument("value", StringType)
      val TestDir =
        Directive("test", arguments = ValueArg :: Nil, locations = Set(DL.FieldDefinition))

      case class CustomIntViolation(value: Int, min: Option[Int], max: Option[Int])
          extends BaseViolation(
            s"Int value must be ${min.fold("")(_.toString + " <= ")}$value${max.fold("")(" <= " + _)}.")

      val MinArg = Argument("min", OptionInputType(IntType))
      val MaxArg = Argument("max", OptionInputType(IntType))
      val ValidateIntDir = Directive(
        "validateInt",
        arguments = MinArg :: MaxArg :: Nil,
        locations = Set(DL.ArgumentDefinition, DL.InputFieldDefinition))

      def intValidationAlias(min: Option[Int], max: Option[Int]) = ScalarAlias[Int, Int](
        IntType,
        toScalar = identity,
        fromScalar = v =>
          if (min.isDefined && v < min.get) Left(CustomIntViolation(v, min, max))
          else if (max.isDefined && v > max.get) Left(CustomIntViolation(v, min, max))
          else Right(v)
      )

      val NVArg = Argument("v", IntType)

      val CoolDir = Directive("cool", locations = Set(DL.Scalar))
      val NumDir = Directive("num", arguments = NVArg :: Nil, locations = Set(DL.Schema, DL.Object))
      val AddExtraFieldsDir = Directive("addExtraFields", locations = Set(DL.Object))

      val builder = resolverBased[Any](
        AdditionalTypes(UUIDType),
        DirectiveFieldProvider(
          AddExtraFieldsDir,
          c =>
            List(
              MaterializedField(
                c.origin,
                ast.FieldDefinition("extraField", ast.NamedType("Int"), Vector.empty)))),
        DynamicDirectiveFieldProvider[Any, JsValue](
          "addExtraDynFields",
          c =>
            List(
              MaterializedField(
                c.origin,
                Field(
                  "extraDynField",
                  c.materializer.getScalarType(c.origin, ast.NamedType("String")),
                  resolve = (_: Context[Any, Any]) => "foo")))
        ),
        AdditionalDirectives(Seq(NumDir)),
        DirectiveInputTypeResolver(
          ValidateIntDir,
          c =>
            c.withArgs(MinArg, MaxArg)((min, max) =>
              c.inputType(c.definition.valueType, intValidationAlias(min, max)))),
        DirectiveScalarResolver(CoolDir, _ => StringType),
        DirectiveResolver(TestDir, resolve = _.arg[String](ValueArg)),
        DynamicDirectiveResolver[Any, JsValue]("json", resolve = _.args),
        FieldResolver(
          { case (TypeName("Query"), FieldName("id")) =>
            _ => UUID.fromString("a26bdfd4-0fcf-484f-b363-585091b3319f")
          },
          PartialFunction.empty),
        AnyFieldResolver.defaultInput[Any, JsValue]
      )

      val schemaAst =
        gql"""
          type Person @addExtraFields {
            name: String!
            size: String
          }

          type Query @num(v: 22) {
            aaa: String @test(value: "foo")
            id(age: Int @validateInt(min: 2, max: 10)): UUID
            b: Person @json(name: "Oleg", extraDynField: "eca12748-ba8c-4dfd-951d-6e1fa21cc31e")
            c: Hello @test(value: "bar")
          }

          extend type Person @addExtraDynFields {
            foo: Int
          }

          scalar Hello @cool

          schema @num(v: 123) {
            query: Query
          }
        """

      val collectedValue = DocumentAnalyzer(schemaAst)
        .resolveDirectives(GenericDirectiveResolver(NumDir, resolve = c => Some(c.arg(NVArg))))
        .sum

      collectedValue should be(145)

      val schema = Schema.buildFromAst(schemaAst, builder.validateSchemaWithException(schemaAst))

      val query =
        gql"""
          {
            id(age: 5)
            c
            aaa
            b {
              name
              size
              extraField
              extraDynField
            }

          }
        """

      Executor.execute(schema, query).await should be("""
        {
          "data": {
            "id": "a26bdfd4-0fcf-484f-b363-585091b3319f",
            "c": "bar",
            "aaa": "foo",
            "b": {
              "name": "Oleg",
              "size": null,
              "extraField": null,
              "extraDynField": "eca12748-ba8c-4dfd-951d-6e1fa21cc31e"
            }
          }
        }
        """.parseJson)

      val introspectionQuery =
        gql"""
          {
            __type(name: "Query") {
              fields {
                name
                type {
                  kind
                  name
                  ofType {
                    kind
                    name
                  }
                }
              }
            }
            __schema {
              types {
                name
              }
            }
          }
        """

      Executor.execute(schema, introspectionQuery).await should be("""
        {
          "data": {
            "__type": {
              "fields": [{
                "name": "aaa",
                "type": {
                  "kind": "SCALAR",
                  "name": "String",
                  "ofType": null
                }
              }, {
                "name": "id",
                "type": {
                  "kind": "SCALAR",
                  "name": "UUID",
                  "ofType": null
                }
              }, {
                "name": "b",
                "type": {
                  "kind": "OBJECT",
                  "name": "Person",
                  "ofType": null
                }
              }, {
                "name": "c",
                "type": {
                  "kind": "SCALAR",
                  "name": "String",
                  "ofType": null
                }
              }]
            },
            "__schema": {
              "types": [{
                "name": "Int"
              }, {
                "name": "Person"
              }, {
                "name": "Query"
              }, {
                "name": "UUID"
              }, {
                "name": "__Directive"
              }, {
                "name": "__DirectiveLocation"
              }, {
                "name": "__EnumValue"
              }, {
                "name": "__Field"
              }, {
                "name": "__InputValue"
              }, {
                "name": "__Schema"
              }, {
                "name": "__Type"
              }, {
                "name": "__TypeKind"
              }, {
                "name": "Boolean"
              }, {
                "name": "String"
              }]
            }
          }
        }
        """.parseJson)
    }

    "resolve fields based on the directives" in {
      val ValueArg: Argument[String] = Argument("value", StringType)

      val ConstDir =
        Directive("const", arguments = ValueArg :: Nil, locations = Set(DL.FieldDefinition))
      val AddDir =
        Directive("add", arguments = ValueArg :: Nil, locations = Set(DL.FieldDefinition))
      val AddFinalDir = Directive("addFinal", locations = Set(DL.Schema, DL.FieldDefinition))

      val builder = resolverBased[Any](
        DirectiveResolver(ConstDir, c => c.arg[String](ValueArg)),
        DirectiveResolver(
          AddDir,
          c =>
            c.withArgs(ValueArg) { value =>
              c.lastValue match {
                case Some(last) => last.map(_.toString + value)
                case None => value
              }
            }),
        DirectiveResolver(
          AddFinalDir,
          c => {
            val finalValue = c.ctx.arg[String]("final")

            c.lastValue match {
              case Some(last) => last.map(_.toString + finalValue)
              case None => finalValue
            }
          },
          complexity = Some(_ => (_, _, _) => 100.0)
        )
      )

      val schemaAst =
        gql"""
          type Query {
            myStr(final: String!): String @const(value: "first") @add(value: "-second") @addFinal
            myStr1(final: String!): String @addFinal @add(value: "second")
          }
        """

      val schema = Schema.buildFromAst(schemaAst, builder.validateSchemaWithException(schemaAst))

      val query =
        gql"""
          {
            myStr(final: "-last")
            myStr1(final: "realFirst-")
          }
        """

      val complexity = new AtomicInteger(0)
      val reducer = QueryReducer.measureComplexity[Any]((c, _) => complexity.set(c.toInt))

      Executor.execute(schema, query, queryReducers = reducer :: Nil).await should be(
        Map("data" -> Map("myStr" -> "first-second-last", "myStr1" -> "realFirst-second")))

      complexity.get should be(200)
    }

    "resolve enum values" in {
      val builder = resolverBased[Any](
        SimpleEnumValueResolver {
          case (TypeName("Color"), v) if v.name == "RED" => "#FF0000"
          case (TypeName("Color"), v) if v.name == "GREEN" => "#00FF00"
          case (TypeName("Color"), v) if v.name == "BLUE" => "#0000FF"
        },
        FieldResolver(
          { case (TypeName("Mutation"), FieldName("eat")) =>
            ctx =>
              "tasty " + ctx.arg[String]("color") + " " + ctx.arg[InputObjectType.DefaultInput](
                "fruit")("color")
          },
          PartialFunction.empty
        )
      )

      val schemaAst =
        gql"""
          enum Color {
            RED, GREEN, BLUE
          }

          input Fruit {
            name: String!
             color: Color!
          }

          extend type Mutation {
            eat(fruit: Fruit!, color: Color!): String
          }
        """

      val existingSchema = Schema(
        query = ObjectType(
          "Query",
          fields[Any, Unit](Field("testQuery", StringType, resolve = _ => "test"))),
        mutation = Some(
          ObjectType(
            "Mutation",
            fields[Any, Unit](Field("testMut", StringType, resolve = _ => "test"))))
      )

      val schema = existingSchema.extend(schemaAst, builder.validateSchemaWithException(schemaAst))

      val query =
        gql"""
          mutation {
            testMut
            eat(fruit: {name: "Apple", color: RED}, color: GREEN)
          }
        """

      Executor.execute(schema, query).await should be(
        Map("data" -> Map("testMut" -> "test", "eat" -> "tasty #00FF00 #FF0000")))

      val queryWithVars =
        gql"""
          mutation Eat($$color1: Color!, $$color2: Color!, $$fruit: Fruit!) {
            eat(fruit: {name: "Apple", color: $$color1}, color: $$color2)
            more: eat(fruit: $$fruit, color: RED)
          }
        """

      val vars = InputUnmarshaller.mapVars(
        "color1" -> "RED",
        "color2" -> "BLUE",
        "fruit" -> Map("name" -> "Banana", "color" -> "GREEN"))

      Executor.execute(schema, queryWithVars, variables = vars).await should be(
        Map("data" -> Map("eat" -> "tasty #0000FF #FF0000", "more" -> "tasty #FF0000 #00FF00")))
    }

    "resolve fields based on the dynamic directives" in {
      import sangria.marshalling.sprayJson._

      val builder = resolverBased[Any](
        DynamicDirectiveResolver[Any, JsValue](
          "add",
          c =>
            c.args.asJsObject.fields("value") match {
              case JsString(str) =>
                c.lastValue match {
                  case Some(last) => last.map(_.toString + str)
                  case None => str
                }
              case _ => c.lastValue.getOrElse("")
            }
        ),
        DynamicDirectiveResolver[Any, JsValue](
          "addFinal",
          c => {
            val finalValue = c.ctx.arg[String]("final")

            c.lastValue match {
              case Some(last) => last.map(_.toString + finalValue)
              case None => finalValue
            }
          },
          complexity = Some(_ => (_, _, _) => 100.0)
        )
      )

      val schemaAst =
        gql"""
          type Query {
            myStr(final: String!): String @add(value: "first") @addFinal
            myStr1(final: String!): String @addFinal @add(value: "second")
          }
        """

      val schema = Schema.buildFromAst(schemaAst, builder.validateSchemaWithException(schemaAst))

      val query =
        gql"""
          {
            myStr(final: "-last")
            myStr1(final: "realFirst-")
          }
        """

      val complexity = new AtomicInteger(0)
      val reducer = QueryReducer.measureComplexity[Any]((c, _) => complexity.set(c.toInt))

      Executor.execute(schema, query, queryReducers = reducer :: Nil).await should be("""
          {
            "data": {
              "myStr": "first-last",
              "myStr1": "realFirst-second"
            }
          }
        """.parseJson)

      complexity.get should be(200)
    }

    "resolve fields based on names" in {
      val builder = resolverBased[Unit](
        FieldResolver(
          {
            case (TypeName("Query"), field @ FieldName(fieldName))
                if fieldName.startsWith("test") =>
              c => c.arg[Int](field.arguments.head.name) + 1
          },
          PartialFunction.empty
        ),
        FieldResolver.map("Query" -> Map("a" -> (_ => "a value"), "b" -> (_ => "b value"))),
        ExistingFieldResolver {
          case (_, _, field) if field.name.startsWith("existing") =>
            _ => "replacement"
        },
        ExistingFieldResolver.map("Query" -> Map("c" -> (_ => "c value")))
      )

      val existingSchema = Schema(
        ObjectType(
          "Query",
          fields[Unit, Unit](
            Field("simple", StringType, resolve = _ => "value"),
            Field("c", StringType, resolve = _ => "c value"),
            Field("existingField", StringType, resolve = _ => "foo"))
        ))

      val schemaAst =
        gql"""
          extend type Query {
            a: String
            b: String
            testOne(size: Int!): Int
            testTwo(size: Int!): Int
          }
        """

      val schema = existingSchema.extend(schemaAst, builder.validateSchemaWithException(schemaAst))

      val query =
        gql"""
          {
            existingField
            simple
            testOne(size: 123)
            testTwo(size: 1)
            a
            b
            c
          }
        """

      Executor.execute(schema, query).await should be(
        Map(
          "data" -> Map(
            "simple" -> "value",
            "existingField" -> "replacement",
            "testOne" -> 124,
            "testTwo" -> 2,
            "a" -> "a value",
            "b" -> "b value",
            "c" -> "c value")))
    }

    "support instance check" in {
      import sangria.marshalling.sprayJson._

      val builder = resolverBased[Unit](
        InstanceCheck.simple {
          case value: JsValue if value.asJsObject.fields.contains("type") =>
            value.asJsObject.fields("type").asInstanceOf[JsString].value
          case value: JsValue if value.asJsObject.fields.contains("name") =>
            "Dog"
          case _ =>
            "Cat"
        },
        AnyFieldResolver.defaultInput[Unit, JsValue]
      )

      val schemaAst =
        gql"""
          enum Color {
            Red, Green, Blue
          }

          interface Fruit {
            id: ID!
          }

          type Apple implements Fruit {
            id: ID!
            color: Color
          }

          type Banana implements Fruit {
            id: ID!
            length: Int
          }

          type Dog {
            name: String!
          }

          type Cat {
            size: Int
          }

          union Pet = Dog | Cat

          type Query {
            fruits: [Fruit]
            pets: [Pet]
          }
        """

      val schema = Schema.buildFromAst(schemaAst, builder.validateSchemaWithException(schemaAst))

      val query =
        gql"""
          {
            fruits {
              __typename
              id

              ... on Apple {color}
              ... on Banana {length}
            }
            pets {
              __typename

              ... on Dog {name}
              ... on Cat {size}
            }
          }
        """

      val data =
        """
          {
            "fruits": [{
              "type": "Apple",
              "id": "1",
              "color": "Red"
            }, {
              "type": "Banana",
              "id": "2",
              "length": 12
            }],
            "pets": [{
              "name": "foo"
            }, {
              "size": 50
            }]
          }
        """.parseJson

      Executor.execute(schema, query, root = data).await should be("""
          {
            "data": {
              "fruits": [{
                "__typename": "Apple",
                "id": "1",
                "color": "Red"
              }, {
                "__typename": "Banana",
                "id": "2",
                "length": 12
              }],
              "pets": [{
                "__typename": "Dog",
                "name": "foo"
              }, {
                "__typename": "Cat",
                "size": 50
              }]
            }
          }
        """.parseJson)
    }

    "support field-based instance check" in {
      import sangria.marshalling.sprayJson._

      val builder = resolverBased[Unit](
        InstanceCheck.field[Unit, JsValue],
        AnyFieldResolver.defaultInput[Unit, JsValue])

      val schemaAst =
        gql"""
          type Dog {
            name: String!
          }

          type Cat {
            size: Int
          }

          union Pet = Dog | Cat

          type Query {
            pets: [Pet]
          }
        """

      val schema = Schema.buildFromAst(schemaAst, builder.validateSchemaWithException(schemaAst))

      val query =
        gql"""
          {
            pets {
              __typename

              ... on Dog {name}
              ... on Cat {size}
            }
          }
        """

      val data =
        """
          {
            "pets": [{
              "type": "Dog",
              "name": "foo"
            }, {
              "type": "Cat",
              "size": 50
            }]
          }
        """.parseJson

      Executor.execute(schema, query, root = data).await should be("""
          {
            "data": {
              "pets": [{
                "__typename": "Dog",
                "name": "foo"
              }, {
                "__typename": "Cat",
                "size": 50
              }]
            }
          }
        """.parseJson)
    }

    "support mutations execution" in {
      import sangria.marshalling.sprayJson._

      val schemaDocument =
        graphql"""
          type Query {
            person: Person!
          }

          type Person {
            name: String!
          }

          type Mutation {
            createPerson(name:String): Person
          }

          schema {
            query: Query
            mutation: Mutation
          }
        """

      val query =
        graphql"""
          mutation {
            createPerson(name: "Hello World") {
              name
            }
          }
        """

      val builder = resolverBased[Any](
        FieldResolver(
          {
            case (_, FieldName("name")) => _ => "test"
            case (_, _) => _ => ()
          },
          PartialFunction.empty))

      val resolverBuilder = builder.validateSchemaWithException(schemaDocument)

      val schema: Schema[Any, Any] =
        Schema.buildFromAst[Any](schemaDocument, resolverBuilder)

      Executor.execute(schema, query).await should be("""
          {
            "data": {
              "createPerson": {
                "name": "test"
              }
            }
          }
        """.parseJson)
    }

    "validate directives" in {
      val schemaDocument =
        graphql"""
          type Query @objectDir(name: "foo") {
            person: String!
          }

          extend type Query @objectDir(name: true) {
            person: String! @objectDir
          }

          extend input Foo @objectDir(name: "wrong")
        """

      val NameArg = Argument("name", OptionInputType(StringType))
      val TestDir = Directive("objectDir", arguments = NameArg :: Nil, locations = Set(DL.Object))

      val builder = resolverBased[Any](AdditionalDirectives(Seq(TestDir)))

      val violations = builder.validateSchema(schemaDocument)

      assertViolations(
        violations,
        "Directive 'objectDir' may not be used on field definition." -> Seq(Pos(7, 29)),
        "Expected type 'String', found 'true'. String value expected" -> Seq(Pos(6, 46)),
        "Directive 'objectDir' may not be used on input object type extension definition." -> Seq(
          Pos(10, 28))
      )
    }

    "support generic InputTypeResolver/OutputTypeResolver" in {
      import sangria.marshalling.sprayJson._

      case object EmptyIdViolation extends BaseViolation("ID cannot be an empty string")
      case object EmptyIdError
          extends Exception("ID cannot be an empty string")
          with UserFacingError

      val MyIdType = ScalarAlias[String, String](
        IDType,
        toScalar = s =>
          if (s.trim.isEmpty) throw EmptyIdError // sanity check
          else s,
        fromScalar = id =>
          if (id.trim.isEmpty) Left(EmptyIdViolation)
          else Right(id))

      val builder = resolverBased[Unit](
        InputTypeResolver {
          case c if c.definition.valueType.namedType.name == "ID" =>
            c.inputType(c.definition.valueType, MyIdType)
        },
        OutputTypeResolver {
          case c if c.fieldDefinition.fieldType.namedType.name == "ID" =>
            c.outputType(c.fieldDefinition.fieldType, MyIdType)
        },
        AnyFieldResolver.defaultInput[Unit, JsValue]
      )

      val schemaAst =
        gql"""
          type Article {
            id: ID
            name: String
          }

          type Query {
            article(ids: [ID!]): [Article]
          }
        """

      val schema = Schema.buildFromAst(schemaAst, builder.validateSchemaWithException(schemaAst))

      val query =
        gql"""
          query Foo($$id: ID!) {
            article(ids: ["test", "", "  ", $$id]) {id}
          }
        """

      val data =
        """
          {
            "article": [{
              "id": "1",
              "name": "foo"
            }, {
              "id": "   ",
              "name": "bar"
            }]
          }
        """.parseJson

      val vars = InputUnmarshaller.mapVars("id" -> "   ")

      checkContainsViolations(
        Executor.execute(schema, query, variables = vars, root = data).await,
        "Expected type 'ID!', found '\"\"'. ID cannot be an empty string" -> Seq(Pos(3, 35)),
        "Expected type 'ID!', found '\"  \"'. ID cannot be an empty string" -> Seq(Pos(3, 39))
      )

      val query1 =
        gql"""
          query Foo($$id: ID!) {
            article(ids: ["test", $$id]) {id}
          }
        """

      Executor.execute(schema, query1, variables = vars, root = data).await should be("""
          {
            "data": {
              "article": null
            },
            "errors": [{
              "message": "Field '$id' has wrong value: ID cannot be an empty string. (line 2, column 21):\n          query Foo($id: ID!) {\n                    ^",
              "path": ["article"],
              "locations": [{"line": 2, "column": 21}]
            }]
          }
        """.parseJson)

      val query2 =
        gql"""
          {
            article(ids: "test") {id name}
          }
        """

      Executor.execute(schema, query2, variables = vars, root = data).await should be("""
          {
            "data": {
              "article": [{
                "id": "1",
                "name": "foo"
              }, {
                "id": null,
                "name": "bar"
              }]
            },
            "errors": [{
              "message": "ID cannot be an empty string",
              "path": ["article", 1, "id"],
              "locations": [{"line": 3, "column": 35}]
            }]
          }
        """.parseJson)
    }

    "validate unique directives on extensions" in {
      val TestDir = Directive(
        "dir",
        locations =
          Set(DL.Object, DL.Interface, DL.Union, DL.InputObject, DL.Scalar, DL.Schema, DL.Enum))

      val TestRepDir = Directive(
        "dir",
        repeatable = true,
        locations =
          Set(DL.Object, DL.Interface, DL.Union, DL.InputObject, DL.Scalar, DL.Schema, DL.Enum))

      val builder = resolverBased[Unit](AdditionalDirectives(Seq(TestDir)))
      val builderRep = resolverBased[Unit](AdditionalDirectives(Seq(TestRepDir)))

      def testForUnique(schemaAst: ast.Document) = {
        val error = intercept[SchemaValidationException](
          Schema.buildFromAst(schemaAst, builder.validateSchemaWithException(schemaAst)))

        error.violations should have size 1

        error.violations.head.errorMessage should include(
          "The directive 'dir' can only be used once at this location.")

        // should be successful
        Schema.buildFromAst(schemaAst, builderRep.validateSchemaWithException(schemaAst))
      }

      testForUnique(gql"""
          type Query @dir {field: String}
          extend type Query @dir
        """)

      testForUnique(gql"""
          type Query {field: String}
          interface Foo @dir {field: String}
          extend interface Foo @dir
        """)

      testForUnique(gql"""
          type Query @dir {filed: String}
          type Cat {field: String}
          type Dog {field: String}

          union Foo @dir = Cat | Dog

          extend union Foo @dir
        """)

      testForUnique(gql"""
          type Query @dir {filed: String}
          enum Foo @dir {RED, BLUE}
          extend enum Foo @dir
        """)

      testForUnique(gql"""
          type Query @dir {filed: String}
          scalar Foo @dir
          extend scalar Foo @dir
        """)

      testForUnique(gql"""
          type Query @dir {filed: String}
          input Foo @dir {field: String}
          extend input Foo @dir
        """)

      testForUnique(gql"""
          type Query @dir {filed: String}
          schema @dir {query: Query}
          extend schema @dir
        """)
    }

    "validate unique directives on extensions with schema extension" in {
      val TestDir = Directive(
        "dir",
        locations =
          Set(DL.Object, DL.Interface, DL.Union, DL.InputObject, DL.Scalar, DL.Schema, DL.Enum))

      val builder = resolverBased[Unit](AdditionalDirectives(Seq(TestDir)))

      val mainSchemaAst = gql"type Query @dir {field: String}"
      val extSchemaAst = gql"extend type Query @dir"

      val schema =
        Schema.buildFromAst(mainSchemaAst, builder.validateSchemaWithException(mainSchemaAst))

      val error = intercept[SchemaValidationException](
        schema.extend(extSchemaAst, builder.validateSchemaWithException(extSchemaAst)))

      error.violations should have size 1
      error.violations.head.errorMessage should include(
        "The directive 'dir' can only be used once at this location.")
    }

    "allow empty field list on types as long as provider contributes new fields" in {
      val TestDir = Directive("dir", locations = Set(DL.Object))

      val builder = resolverBased[Any](DirectiveFieldProvider(
        TestDir,
        c =>
          List(MaterializedField(
            c.origin,
            Field("hello", c.scalarType("String"), resolve = (_: Context[Any, Any]) => "world")))))

      val schemaAst = gql"type Query @dir"

      val schema = Schema.buildFromAst(schemaAst, builder.validateSchemaWithException(schemaAst))

      val query = gql"{hello}"

      Executor.execute(schema, query).await should be(Map("data" -> Map("hello" -> "world")))
    }
  }
}
