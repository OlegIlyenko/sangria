package sangria.macros.derive

import sangria.execution.Executor
import sangria.introspection._
import sangria.marshalling.ScalaInput
import sangria.schema._
import sangria.macros._
import sangria.util.FutureResultSupport
import spray.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Success, Try}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DeriveObjectTypeMacroSpec extends AnyWordSpec with Matchers with FutureResultSupport {
  import DeriveMacroTestModel._

  @GraphQLName("MyFruit")
  @GraphQLDescription("Very tasty fruit")
  sealed trait FruitAnnotated

  @GraphQLName("JustApple")
  @GraphQLDescription("The red one")
  case object RedAppleAnnotated extends FruitAnnotated

  @GraphQLExclude
  case object SuperBananaAnnotated extends FruitAnnotated

  @GraphQLDeprecated("Not tasty anymore")
  case object MegaOrangeAnnotated extends FruitAnnotated

  case class Comment(
      author: String,
      text: Option[String],
      color: ColorAnnotated.Value = ColorAnnotated.Red)
  case class Article(
      title: String,
      text: Option[String],
      tags: Option[Vector[String]],
      comments: Option[Vector[Option[Comment]]],
      fruit: FruitAnnotated = RedAppleAnnotated)

  case class Pet(name: String, size: Option[Int])

  "ObjectType derivation" should {
    "use class name and have no description by default" in {
      val tpe = deriveObjectType[Unit, TestSubject]()

      tpe.name should be("TestSubject")
      tpe.description should be(None)
    }

    "allow to change name and description with config" in {
      val tpe =
        deriveObjectType[Unit, TestSubject](ObjectTypeName("Foo"), ObjectTypeDescription("my desc"))

      tpe.name should be("Foo")
      tpe.description should be(Some("my desc"))
    }

    "allow to change name and description with annotations" in {
      val tpe = deriveObjectType[Unit, TestSubjectAnnotated]()

      tpe.name should be("MyQueryType")
      tpe.description should be(Some("My type!"))
    }

    "prioritize config over annotation for name and description" in {
      val tpe = deriveObjectType[Unit, TestSubjectAnnotated](
        ObjectTypeName("Foo"),
        ObjectTypeDescription("my desc"))

      tpe.name should be("Foo")
      tpe.description should be(Some("my desc"))
    }

    "allow to specify interfaces via config" in {
      val tpe = deriveObjectType[Unit, TestSubject](Interfaces(Parent1Type, Parent2Type))

      tpe.interfaces.map(_.name).sorted should be("Parent1" :: "Parent2" :: Nil)
    }

    "allow to specify interfaces via multiple config entries" in {
      val tpe =
        deriveObjectType[Unit, TestSubject](Interfaces(Parent1Type), Interfaces(Parent2Type))

      tpe.interfaces.map(_.name).sorted should be("Parent1" :: "Parent2" :: Nil)
    }

    "expose case class fields" in {
      val tpe = deriveObjectType[Unit, TestSubject]()
      val fields = tpe.fields

      fields should have size 3

      val idField = fields.find(_.name == "id")
      idField shouldNot be(None)
      idField.get.fieldType should be(StringType)

      val listField = fields.find(_.name == "list")
      listField shouldNot be(None)
      listField.get.fieldType should be(ListType(StringType))

      val excludedField = fields.find(_.name == "excluded")
      excludedField shouldNot be(None)
      excludedField.get.fieldType should be(IntType)
    }

    "validate known field names" in {
      """deriveObjectType[Unit, TestSubject](IncludeFields("id", "list1"))""" shouldNot compile
      """deriveObjectType[Unit, TestSubject](ExcludeFields("id1"))""" shouldNot compile
      """deriveObjectType[Unit, TestSubject](DocumentField("id1", "foo"))""" shouldNot compile
      """deriveObjectType[Unit, TestSubject](RenameField("id1", "foo"))""" shouldNot compile
      """deriveObjectType[Unit, TestSubject](FieldTags("id1", CachedTag))""" shouldNot compile
      """deriveObjectType[Unit, TestSubject](DeprecateField("id1", "test"))""" shouldNot compile
      """deriveObjectType[Unit, TestSubject](FieldComplexity("id1", (_, _, _) => 1.0))""" shouldNot compile
      """deriveObjectType[Unit, TestSubjectAnnotated](ExcludeFields("id", "list", "excluded"))""" shouldNot compile
    }

    "respect whitelist and blacklist provided via config" in {
      val tpe =
        deriveObjectType[Unit, TestSubject](IncludeFields("id", "list"), ExcludeFields("list"))

      tpe.fields should have size 1

      tpe.fields(0).name should be("id")
    }

    "respect blacklist provided via annotations" in {
      val tpe = deriveObjectType[Unit, TestSubjectAnnotated](
        IncludeFields("id", "list", "excluded"),
        ExcludeFields("id"))

      tpe.fields should have size 1
      tpe.fields(0).name should be("myList")
    }

    "allow to add new fields" in {
      val tpe = deriveObjectType[Unit, TestSubject](
        IncludeFields("id"),
        AddFields(
          Field("foo", ListType(StringType), resolve = _.value.list),
          Field("bar", BooleanType, resolve = _ => true)))

      tpe.fields should have size 3

      val idField = tpe.fields.find(_.name == "id")
      idField shouldNot be(None)
      idField.get.fieldType should be(StringType)

      val fooField = tpe.fields.find(_.name == "foo")
      fooField shouldNot be(None)
      fooField.get.fieldType should be(ListType(StringType))

      val barField = tpe.fields.find(_.name == "bar")
      barField shouldNot be(None)
      barField.get.fieldType should be(BooleanType)
    }

    "allow to override fields" in {
      val tpe = deriveObjectType[Unit, TestSubject](
        ReplaceField("id", Field("id", ListType(StringType), resolve = _.value.list)),
        ReplaceField("list", Field("bar", BooleanType, resolve = _ => true)))

      tpe.fields should have size 3

      val excludedField = tpe.fields.find(_.name == "excluded")
      excludedField shouldNot be(None)
      excludedField.get.fieldType should be(IntType)

      val idField = tpe.fields.find(_.name == "id")
      idField shouldNot be(None)
      idField.get.fieldType should be(ListType(StringType))

      val barField = tpe.fields.find(_.name == "bar")
      barField shouldNot be(None)
      barField.get.fieldType should be(BooleanType)
    }

    "allow to set field complexity with config" in {
      val tpe =
        deriveObjectType[Unit, TestSubject](FieldComplexity("id", (_, _, child) => child * 123.0))
      val idField = tpe.fields.find(_.name == "id")
      idField shouldNot be(None)
      idField.get.complexity.get((), Args.empty, 2d) should be(246.0)
    }

    "allow to set name, description, deprecationReason and fieldTags with config" in {
      val tpe = deriveObjectType[Unit, TestSubject](
        DocumentField("id", "the object ID", deprecationReason = Some("foo")),
        RenameField("id", "identifier"),
        RenameField("list", "colors"),
        DocumentField("list", "my colors"),
        DeprecateField("excluded", "bar"),
        FieldTags("list", CachedTag, AuthorizedTag)
      )

      tpe.fields should have size 3

      val identifierField = tpe.fields.find(_.name == "identifier")
      identifierField shouldNot be(None)
      identifierField.get.description should be(Some("the object ID"))
      identifierField.get.deprecationReason should be(Some("foo"))
      identifierField.get.tags should be(Nil)
      identifierField.get.fieldType should be(StringType)

      val colorField = tpe.fields.find(_.name == "colors")
      colorField shouldNot be(None)
      colorField.get.description should be(Some("my colors"))
      colorField.get.deprecationReason should be(None)
      colorField.get.tags should be(List(CachedTag, AuthorizedTag))
      colorField.get.fieldType should be(ListType(StringType))

      val excludedField = tpe.fields.find(_.name == "excluded")
      excludedField shouldNot be(None)
      excludedField.get.description should be(None)
      excludedField.get.deprecationReason should be(Some("bar"))
      excludedField.get.tags should be(Nil)
      excludedField.get.fieldType should be(IntType)
    }

    "allow field names transformation" in {
      val tpe = deriveObjectType[Unit, TestSubjectAnnotated](TransformFieldNames(_.toUpperCase))

      tpe.fields.map(_.name) should (have(size(2)).and(contain("ID")).and(contain("MYLIST")))

      val transformer2 = (s: String) =>
        s.zipWithIndex
          .map {
            case (c, i) if i % 2 == 0 => c.toLower
            case (c, _) => c.toUpper
          }
          .mkString("")

      val tpe2 = deriveObjectType[Unit, TestSubjectAnnotated](TransformFieldNames(transformer2))

      tpe2.fields.map(_.name) should (have(size(2)).and(contain("iD")).and(contain("mYlIsT")))
    }

    "allow to set name, description, deprecationReason and fieldTags with annotations" in {
      val tpe = deriveObjectType[Unit, TestSubjectAnnotated]()

      tpe.fields should have size 2

      val idField = tpe.fields.find(_.name == "id")
      idField shouldNot be(None)
      idField.get.description should be(Some("my id"))
      idField.get.deprecationReason should be(Some("No IDs anymore!"))
      idField.get.tags should be(Nil)
      idField.get.fieldType should be(IDType)

      val myListField = tpe.fields.find(_.name == "myList")
      myListField shouldNot be(None)
      myListField.get.description should be(None)
      myListField.get.deprecationReason should be(None)
      myListField.get.tags should be(List(CachedTag, AuthorizedTag))
      myListField.get.fieldType should be(ListType(StringType))
    }

    "prioritize field config name, description, deprecationReason and merge fieldTags" in {
      val tpe = deriveObjectType[Unit, TestSubjectAnnotated](
        RenameField("list", "fooBar"),
        DocumentField("id", "new descr", Some("new depr")),
        FieldTags("id", FooTag),
        FieldTags("list", FooTag))

      tpe.fields should have size 2

      val idField = tpe.fields.find(_.name == "id")
      idField shouldNot be(None)
      idField.get.description should be(Some("new descr"))
      idField.get.deprecationReason should be(Some("new depr"))
      idField.get.tags should be(List(FooTag))
      idField.get.fieldType should be(IDType)

      val fooBarField = tpe.fields.find(_.name == "fooBar")
      fooBarField shouldNot be(None)
      fooBarField.get.description should be(None)
      fooBarField.get.deprecationReason should be(None)
      fooBarField.get.tags should be(List(FooTag, CachedTag, AuthorizedTag))
      fooBarField.get.fieldType should be(ListType(StringType))
    }

    "support overriding field types" in {
      class TestObject {
        @GraphQLField
        @GraphQLOutputType(IDType)
        val id: String = ""
      }

      val tpe = deriveObjectType[Unit, TestObject]()

      tpe.fields should have size 1

      tpe.fields(0).name should be("id")
      tpe.fields(0).fieldType should be(IDType)
    }

    "support overriding field types with optionals" in {
      class TestObject {
        @GraphQLField
        @GraphQLOutputType(OptionType(IDType))
        val id: Option[String] = None
      }

      val tpe = deriveObjectType[Unit, TestObject]()

      tpe.fields should have size 1

      tpe.fields(0).name should be("id")
      tpe.fields(0).fieldType should be(OptionType(IDType))
    }

    "overwriting type should work even if implicit type is not found" in {
      class InnerClass {
        @GraphQLField
        val someValue: String = "foo"
      }

      val innerClassType = deriveObjectType[Unit, InnerClass]()

      class TestObject {
        @GraphQLField
        @GraphQLOutputType(OptionType(innerClassType))
        val inner: Option[InnerClass] = None
      }

      val tpe = deriveObjectType[Unit, TestObject]()

      tpe.fields should have size 1

      tpe.fields(0).name should be("inner")
      tpe.fields(0).fieldType should be(OptionType(innerClassType))
    }

    "support vals" in {
      class MyTest {
        @GraphQLField
        @GraphQLName("foo")
        @GraphQLDescription("test field")
        val bar: Option[List[Int]] = Some(List(1, 2, 3))
      }

      val tpe = deriveObjectType[Unit, MyTest]()

      tpe.fields should have size 1

      tpe.fields(0).name should be("foo")
      tpe.fields(0).description should be(Some("test field"))
      tpe.fields(0).deprecationReason should be(None)
      tpe.fields(0).fieldType should be(OptionType(ListType(IntType)))
    }

    "support companion objects for `Enumeration`s" in {
      val `enum` = test.AnotherEnum.valNameType

      `enum`.values.map(_.name) should (
        have(size(3)).and(contain("FOO")).and(contain("BAR")).and(contain("BAZ"))
      )
    }

    "be able to find other types via implicit GraphQL types" in {
      implicit val FruitType = deriveEnumType[FruitAnnotated]()
      implicit val ColorType = deriveEnumType[ColorAnnotated.Value]()

      implicit val CommentType = deriveObjectType[Unit, Comment](
        DocumentField("author", "The comment author"),
        DocumentField("text", "Comment text"))

      val ArticleType = deriveObjectType[Unit, Article](RenameField("tags", "myTags"))

      val testArticle = Article(
        "My First Article",
        Some("foo bar"),
        None,
        Some(Vector(Some(Comment("bob", None)), None, Some(Comment("jane", Some("yay!"))))))

      val query =
        graphql"""
          {
            title
            text
            myTags
            fruit
            comments {
              author
              text
              color
            }
          }
        """

      val schema = Schema(ArticleType)

      Executor.execute(schema, query, root = testArticle).await should be(
        Map("data" -> Map(
          "title" -> "My First Article",
          "text" -> "foo bar",
          "myTags" -> null,
          "fruit" -> "JustApple",
          "comments" -> List(
            Map("author" -> "bob", "text" -> null, "color" -> "NormalRed"),
            null,
            Map("author" -> "jane", "text" -> "yay!", "color" -> "NormalRed"))
        )))

      import sangria.marshalling.queryAst._

      val intro = IntrospectionParser
        .parse(
          Executor
            .execute(schema, sangria.introspection.introspectionQuery, root = testArticle)
            .await)
        .get

      intro.queryType.name should be("Article")

      val Some(articleIntro: IntrospectionObjectType) = intro.types.find(_.name == "Article")
      val Some(commentIntro: IntrospectionObjectType) = intro.types.find(_.name == "Comment")

      commentIntro.fields should have size 3

      val authorField = commentIntro.fields.find(_.name == "author")
      authorField shouldNot be(None)
      authorField.get.name should be("author")
      authorField.get.description should be(Some("The comment author"))

      val textField = commentIntro.fields.find(_.name == "text")
      textField shouldNot be(None)
      textField.get.name should be("text")
      textField.get.description should be(Some("Comment text"))

      val colorField = commentIntro.fields.find(_.name == "color")
      colorField shouldNot be(None)
      colorField.get.name should be("color")
      colorField.get.description should be(None)

      val commentsField = articleIntro.fields.find(_.name == "comments")
      commentsField shouldNot be(None)
      commentsField.get.name should be("comments")
      commentsField.get.tpe should be(
        IntrospectionListTypeRef(IntrospectionNamedTypeRef(TypeKind.Object, "Comment")))

      val fruitField = articleIntro.fields.find(_.name == "fruit")
      fruitField shouldNot be(None)
      fruitField.get.name should be("fruit")
      fruitField.get.tpe should be(
        IntrospectionNonNullTypeRef(IntrospectionNamedTypeRef(TypeKind.Enum, "MyFruit")))
    }

    "be able handle recursive types with field overrides" in {
      case class A(id: Int, b: B)
      case class B(name: String, a: A, b: B)

      implicit lazy val AType: ObjectType[Unit, A] =
        deriveObjectType[Unit, A](ReplaceField("b", Field("b", BType, resolve = _.value.b)))

      implicit lazy val BType: ObjectType[Unit, B] = deriveObjectType(
        ReplaceField("a", Field("a", AType, resolve = _.value.a)),
        ReplaceField("b", Field("b", BType, resolve = _.value.b)))

      val schema = Schema(AType)

      val query =
        graphql"{id, b {name, a {id}, b {name}} }"

      Executor
        .execute(schema, query, root = A(1, B("foo", A(2, null), B("bar", null, null))))
        .await should be(
        Map(
          "data" -> Map(
            "id" -> 1,
            "b" -> Map("name" -> "foo", "a" -> Map("id" -> 2), "b" -> Map("name" -> "bar")))))
    }

    "use companion object to resolve derived types" in {
      import test._

      val schema = Schema(CompanionA.graphqlType)

      val query = graphql"{b {myC {e, e1}}}"

      Executor
        .execute(
          schema,
          query,
          root = CompanionA(CompanionB(CompanionC(CompanionEnum1, AnotherEnum.FOO))))
        .await should be(
        Map("data" -> Map("b" -> Map("myC" -> Map("e" -> "first", "e1" -> "FOO")))))
    }

    "support `Future`, `Try`, `Defer` and `Action` return types" in {
      case class MyTest(deferVal: TestDefer) {
        @GraphQLField
        def futureVal: Future[List[Int]] = Future.successful(List(11, 22))

        @GraphQLField
        def tryVal: Try[Option[List[Int]]] = Success(Some(List(33, 44)))

        @GraphQLField
        def actionVal = DeferredFutureValue(Future.successful(TestDefer(1)))
      }

      val tpe = deriveObjectType[Unit, MyTest]()

      tpe.fields.sortBy(_.name).map(f => f.name -> f.fieldType) should be(
        List(
          "actionVal" -> OptionType(ListType(IntType)),
          "deferVal" -> OptionType(ListType(IntType)),
          "futureVal" -> ListType(IntType),
          "tryVal" -> OptionType(ListType(IntType))
        ))
    }

    "derive methods with arguments via annotations" in {
      object MyJsonProtocol extends DefaultJsonProtocol {
        implicit val PetFormat: JsonFormat[Pet] = jsonFormat2(Pet.apply)
      }

      import MyJsonProtocol._
      import sangria.marshalling.sprayJson._

      implicit val PetType = deriveInputObjectType[Pet]()
      implicit val colorType = deriveEnumType[Color.Value]()

      case class Ctx(num: Int, fooBar: FooBar)

      class FooBar {
        @GraphQLField
        @GraphQLName("foo")
        def hello(
            @GraphQLDefault(123)
            id: Int,
            songs: Seq[String]
        )(
            ctx: Context[Ctx, Unit],
            @GraphQLDefault(Pet("xxx", Some(322)))
            pet: Pet,
            @GraphQLName("aaa")
            @GraphQLDescription("bbbb")
            @GraphQLDefault(ScalaInput.scalaInput(List(Color.Red)))
            colors: Seq[Color.Value]
        ) =
          s"id = $id, songs = ${songs.mkString(",")}, cc = ${colors.mkString(",")}, pet = $pet, ctx = ${ctx.ctx.num}"

        @GraphQLField
        def opt(str: Option[String], color: Option[Color.Value])(pet: Option[Pet]) =
          s"str = $str, color = $color, pet = $pet"

        @GraphQLField
        def paramType(
            @GraphQLInputType(IDType) id: String,
            @GraphQLInputType(IDType)
            @GraphQLDefault(47589)
            defaultId: String) =
          s"id = $id, defaultId = $defaultId"

      }

      val tpe = deriveContextObjectType[Ctx, FooBar, Unit](_.fooBar)

      val schema = Schema(tpe)

      val query =
        graphql"""
          {
            foo(songs: ["a", "b"])
            foo1: foo(songs: ["a", "b"], pet: {name: "mypet", size: 156})
            opt
            opt1: opt(str: "test", color: Red, pet: {name: "anotherPet", size: 321})
            paramType(id: 345)
          }
        """

      Executor.execute(schema, query, Ctx(987, new FooBar)).await should be(
        JsObject(
          "data" -> JsObject(
            "foo" -> JsString(
              "id = 123, songs = a,b, cc = Red, pet = Pet(xxx,Some(322)), ctx = 987"),
            "foo1" -> JsString(
              "id = 123, songs = a,b, cc = Red, pet = Pet(mypet,Some(156)), ctx = 987"),
            "opt" -> JsString("str = None, color = None, pet = None"),
            "opt1" -> JsString(
              "str = Some(test), color = Some(Red), pet = Some(Pet(anotherPet,Some(321)))"),
            "paramType" -> JsString("id = 345, defaultId = 47589")
          )))

      val intro = IntrospectionParser
        .parse(Executor.execute(schema, introspectionQuery, Ctx(987, new FooBar)).await)
        .get
      val introType = intro.types.find(_.name == "FooBar").get.asInstanceOf[IntrospectionObjectType]

      introType.fields should have size 3

      val Some(helloField) = introType.fields.find(_.name == "foo")

      helloField.args should have size 4

      helloField.args should be(
        List(
          IntrospectionInputValue(
            "id",
            None,
            IntrospectionNamedTypeRef(TypeKind.Scalar, "Int"),
            Some("123"),
            None,
            None),
          IntrospectionInputValue(
            "songs",
            None,
            IntrospectionNonNullTypeRef(IntrospectionListTypeRef(
              IntrospectionNonNullTypeRef(IntrospectionNamedTypeRef(TypeKind.Scalar, "String")))),
            None,
            None,
            None
          ),
          IntrospectionInputValue(
            "pet",
            None,
            IntrospectionNamedTypeRef(TypeKind.InputObject, "Pet"),
            Some("""{name:"xxx",size:322}"""),
            None,
            None),
          IntrospectionInputValue(
            "aaa",
            Some("bbbb"),
            IntrospectionListTypeRef(
              IntrospectionNonNullTypeRef(IntrospectionNamedTypeRef(TypeKind.Enum, "Color"))),
            Some("[Red]"),
            None,
            None
          )
        ))

      val Some(optField) = introType.fields.find(_.name == "opt")

      optField.args should have size 3

      optField.args should be(
        List(
          IntrospectionInputValue(
            "str",
            None,
            IntrospectionNamedTypeRef(TypeKind.Scalar, "String"),
            None,
            None,
            None),
          IntrospectionInputValue(
            "color",
            None,
            IntrospectionNamedTypeRef(TypeKind.Enum, "Color"),
            None,
            None,
            None),
          IntrospectionInputValue(
            "pet",
            None,
            IntrospectionNamedTypeRef(TypeKind.InputObject, "Pet"),
            None,
            None,
            None)
        ))

      val Some(paramTypeField) = introType.fields.find(_.name == "paramType")

      paramTypeField.args should have size 2
      paramTypeField.args should be(
        Vector(
          IntrospectionInputValue(
            "id",
            None,
            IntrospectionNonNullTypeRef(IntrospectionNamedTypeRef(TypeKind.Scalar, "ID")),
            None,
            None,
            None),
          IntrospectionInputValue(
            "defaultId",
            None,
            IntrospectionNamedTypeRef(TypeKind.Scalar, "ID"),
            Some(""""47589""""),
            None,
            None)
        ))
    }

    "allow to rename arguments, set arguments descriptions and default values with config" in {
      object MyJsonProtocol extends DefaultJsonProtocol {
        implicit val PetFormat: JsonFormat[Pet] = jsonFormat2(Pet.apply)
      }

      import MyJsonProtocol._
      import sangria.marshalling.sprayJson._

      implicit val PetType = deriveInputObjectType[Pet]()
      implicit val colorType = deriveEnumType[Color.Value]()

      case class Ctx(num: Int, fooBar: FooBar)

      class FooBar {
        def hello(
            id: Int,
            songs: Seq[String])(ctx: Context[Ctx, Unit], pet: Pet, colors: Seq[Color.Value]) =
          s"id = $id, songs = ${songs.mkString(",")}, cc = ${colors.mkString(",")}, pet = $pet, ctx = ${ctx.ctx.num}"

        def opt(str: Option[String], color: Option[Color.Value])(pet: Option[Pet]) =
          s"str = $str, color = $color, pet = $pet"
      }

      val tpe = deriveContextObjectType[Ctx, FooBar, Unit](
        _.fooBar,
        IncludeMethods("hello", "opt"),
        MethodArgumentDescription("hello", "id", "`id`"),
        MethodArgumentDescription("hello", "songs", "`songs`"),
        MethodArgumentRename("opt", "str", "description"),
        MethodArgumentsDescription("opt", "str" -> "Optional description", "color" -> "a color"),
        MethodArgumentDefault("hello", "songs", "My favorite song" :: Nil),
        MethodArgumentDefault("opt", "pet", """{"name": "Bell", "size": 3}""".parseJson),
        MethodArgument("hello", "pet", "`pet`", Pet("Octocat", None))
      )

      val schema = Schema(tpe)

      val intro = IntrospectionParser
        .parse(Executor.execute(schema, introspectionQuery, Ctx(987, new FooBar)).await)
        .get
      val introType = intro.types.find(_.name == "FooBar").get.asInstanceOf[IntrospectionObjectType]

      introType.fields should have size 2

      val Some(helloField) = introType.fields.find(_.name == "hello")

      helloField.args should have size 4

      helloField.args should be(
        List(
          IntrospectionInputValue(
            "id",
            Some("`id`"),
            IntrospectionNonNullTypeRef(IntrospectionNamedTypeRef(TypeKind.Scalar, "Int")),
            None,
            None,
            None),
          IntrospectionInputValue(
            "songs",
            Some("`songs`"),
            IntrospectionListTypeRef(
              IntrospectionNonNullTypeRef(IntrospectionNamedTypeRef(TypeKind.Scalar, "String"))),
            Some("""["My favorite song"]"""),
            None,
            None
          ),
          IntrospectionInputValue(
            "pet",
            Some("`pet`"),
            IntrospectionNamedTypeRef(TypeKind.InputObject, "Pet"),
            Some("""{name:"Octocat"}"""),
            None,
            None),
          IntrospectionInputValue(
            "colors",
            None,
            IntrospectionNonNullTypeRef(IntrospectionListTypeRef(
              IntrospectionNonNullTypeRef(IntrospectionNamedTypeRef(TypeKind.Enum, "Color")))),
            None,
            None,
            None
          )
        ))

      val Some(optField) = introType.fields.find(_.name == "opt")

      optField.args should have size 3

      optField.args should be(
        List(
          IntrospectionInputValue(
            "description",
            Some("Optional description"),
            IntrospectionNamedTypeRef(TypeKind.Scalar, "String"),
            None,
            None,
            None),
          IntrospectionInputValue(
            "color",
            Some("a color"),
            IntrospectionNamedTypeRef(TypeKind.Enum, "Color"),
            None,
            None,
            None),
          IntrospectionInputValue(
            "pet",
            None,
            IntrospectionNamedTypeRef(TypeKind.InputObject, "Pet"),
            Some("""{name:"Bell",size:3}"""),
            None,
            None)
        ))
    }

    "validate known argument names" in {
      class Foo { def foo(name: String) = 1 }
      """deriveObjectType[Unit, Foo](IncludeFields("foo"), MethodArgumentDescription("foo", "bar", "???"))""" shouldNot compile
    }
    "validate arguments' default types" in {
      class Foo { def foo(name: String) = 1 }
      """deriveObjectType[Unit, Foo](IncludeFields("foo"), MethodArgumentDefault("foo", "name", 1))""" shouldNot compile
    }

    "not set a default value to `null`" in {
      class Query {
        @GraphQLField def foo(a: Option[String] = None) = "" + a
      }

      import sangria.marshalling.sprayJson._

      val QueryType = deriveObjectType[Unit, Query]()
      val schema = Schema(QueryType)
      val intro = IntrospectionParser
        .parse(
          Executor
            .execute(schema, sangria.introspection.introspectionQuery, root = new Query)
            .await)
        .get

      intro
        .typesByName("Query")
        .asInstanceOf[IntrospectionObjectType]
        .fieldsByName("foo")
        .argsByName("a")
        .defaultValue should be(None)
    }

    "derive object types for type-parameterized classes" in {
      implicit val testSubject = deriveObjectType[Unit, TestSubject]()
      deriveObjectType[Unit, TestContainer[TestSubject]]()

      "deriveObjectType[Unit, TestContainer[TestSubject]]()" should compile
    }

    "handle Future result values correctly (issue #906)" in {
      import Issue906Context._
      import sangria.parser.QueryParser

      val query = QueryParser
        .parse(
          s"""
             query SomeTest {
                 mySample {
                   myFutureString
                 }
             }
            """
        )
        .fold(throw _, identity)

      Executor.execute(MySample.schema, query, MyRepository.sample).await should be(
        Map(
          "data" -> Map(
            "mySample" -> Map(
              "myFutureString" -> "Hello World"
            )
          )
        )
      )
    }
  }
}
