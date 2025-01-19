package sangria.execution

import sangria.ast
import sangria.marshalling.InputUnmarshaller
import sangria.parser.QueryParser
import sangria.schema._
import sangria.macros._
import sangria.util.{FutureResultSupport, Pos}
import sangria.validation.QueryValidator
import InputUnmarshaller.mapVars
import sangria.execution.deferred.{Deferred, DeferredResolver, Fetcher, HasId}
import sangria.util.SimpleGraphQlSupport.checkContainsErrors

import scala.collection.immutable.Map
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success
import scala.concurrent.ExecutionContext.Implicits.global
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import sangria.util.tag.@@ // Scala 3 issue workaround
import sangria.marshalling.FromInput.CoercedScalaResult

class ExecutorSpec extends AnyWordSpec with Matchers with FutureResultSupport {
  class TestSubject {
    def a: Option[String] = Some("Apple")
    def b: Option[String] = Some("Banana")
    def c: Option[String] = Some("Cookie")
    def d: Option[String] = Some("Donut")
    def e: Option[String] = Some("Egg")
    val f: Option[String] = Some("Fish")
    def deep: Option[DeepTestSubject] = Some(new DeepTestSubject)
    def deepColor(c: String): DeepTestSubject = new DeepTestSubject(c)
    def pic(size: Option[Int]) = "Pic of size: " + (size.getOrElse(50))
    def future: Future[Option[TestSubject]] = Future.successful(Some(new TestSubject))
  }

  class DeepTestSubject(val color: String = "none") {
    def a: Option[String] = Some("Already Been Done")
    def b: Option[String] = Some("Boring")
    def c: List[Option[String]] = Some("Contrived") :: None :: Some("Confusing") :: Nil
    def deeper: List[Option[TestSubject]] =
      Some(new TestSubject) :: null :: Some(new TestSubject) :: Nil
  }

  case class Ctx(color: String = "green")

  case class LightColor(subj: TestSubject, color: String) extends Deferred[DeepTestSubject]
  case class FailColor(subj: TestSubject, color: String) extends Deferred[DeepTestSubject]

  class LightColorResolver extends DeferredResolver[Any] {
    def resolve(deferred: Vector[Deferred[Any]], ctx: Any, queryState: Any)(implicit
        ec: ExecutionContext) = deferred.map {
      case LightColor(v, c) => Future.successful(v.deepColor("light" + c))
      case FailColor(v, c) => Future.failed(new IllegalStateException("error in resolver"))
    }
  }

  class BrokenLightColorResolver extends DeferredResolver[Any] {
    def resolve(deferred: Vector[Deferred[Any]], ctx: Any, queryState: Any)(implicit
        ec: ExecutionContext) = (deferred ++ deferred).map {
      case LightColor(v, c) => Future.successful(v.deepColor("light" + c))
      case FailColor(v, c) => Future.failed(new IllegalStateException("error in resolver"))
    }
  }

  val DeepDataType = ObjectType(
    "DeepDataType",
    () =>
      fields[Ctx, DeepTestSubject](
        Field("a", OptionType(StringType), resolve = _.value.a),
        Field("b", OptionType(StringType), resolve = _.value.b),
        Field("c", OptionType(ListType(OptionType(StringType))), resolve = _.value.c),
        Field("color", StringType, resolve = _.value.color),
        Field("ctxColor", OptionType(StringType), resolve = _.ctx.color),
        Field("deeper", OptionType(ListType(OptionType(DataType))), resolve = _.value.deeper)
      )
  )

  val DataType: ObjectType[Ctx, TestSubject] = ObjectType(
    "DataType",
    () =>
      fields[Ctx, TestSubject](
        Field("a", OptionType(StringType), resolve = _.value.a),
        Field("b", OptionType(StringType), resolve = _.value.b),
        Field("c", OptionType(StringType), resolve = _.value.c),
        Field("d", OptionType(StringType), resolve = _.value.d),
        Field("e", OptionType(StringType), resolve = _.value.e),
        Field("f", OptionType(StringType), resolve = _.value.f),
        Field(
          "ctxUpdating",
          DeepDataType,
          resolve =
            ctx => UpdateCtx(ctx.value.deepColor("blue"))(v => ctx.ctx.copy(color = v.color))),
        Field(
          "ctxUpdatingFut",
          DeepDataType,
          resolve = ctx =>
            UpdateCtx(Future.successful(ctx.value.deepColor("orange")))(v =>
              ctx.ctx.copy(color = v.color))),
        Field(
          "ctxUpdatingDef",
          DeepDataType,
          resolve =
            ctx => UpdateCtx(LightColor(ctx.value, "magenta"))(v => ctx.ctx.copy(color = v.color))),
        Field(
          "ctxUpdatingDefFut",
          DeepDataType,
          resolve = ctx =>
            UpdateCtx(DeferredFutureValue(Future.successful(LightColor(ctx.value, "red"))))(v =>
              ctx.ctx.copy(color = v.color))
        ),
        Field("def", DeepDataType, resolve = ctx => LightColor(ctx.value, "magenta")),
        Field(
          "defFut",
          DeepDataType,
          resolve = ctx => DeferredFutureValue(Future.successful(LightColor(ctx.value, "red")))),
        Field(
          "defFail",
          OptionType(DeepDataType),
          resolve = ctx => FailColor(ctx.value, "magenta")),
        Field(
          "defFutFail",
          OptionType(DeepDataType),
          resolve = ctx => DeferredFutureValue(Future.successful(FailColor(ctx.value, "red")))),
        Field(
          "pic",
          OptionType(StringType),
          arguments =
            Argument[Option[Int @@ CoercedScalaResult]]("size", OptionInputType(IntType)) :: Nil,
          resolve = ctx => ctx.value.pic(ctx.argOpt[Int]("size"))
        ),
        Field("deep", OptionType(DeepDataType), resolve = _.value.deep),
        Field("future", OptionType(DataType), resolve = _.value.future)
      )
  )

  val ParallelFragmentType: ObjectType[Unit, Unit] = ObjectType(
    "Type",
    () =>
      fields[Unit, Unit](
        Field("a", OptionType(StringType), resolve = _ => "Apple"),
        Field("b", OptionType(StringType), resolve = _ => "Banana"),
        Field("c", OptionType(StringType), resolve = _ => "Cherry"),
        Field("d", StringType, resolve = _ => "Door"),
        Field("deep", OptionType(ParallelFragmentType), resolve = _ => ())
      )
  )

  "Execute: Handles basic execution tasks" should {
    "execute arbitrary queries" in {
      val doc = gql"""
        query Example($$size: Int) {
          a,
          b,
          x: c
          ...c
          f
          ...on DataType {
            pic(size: $$size)
            future {
              a
            }
          }
          deep {
            a
            b
            c
            deeper {
              a
              b
            }
          }
        }

        fragment c on DataType {
          d
          e
        }
      """

      val expected = Map(
        "data" -> Map(
          "a" -> "Apple",
          "b" -> "Banana",
          "x" -> "Cookie",
          "d" -> "Donut",
          "e" -> "Egg",
          "f" -> "Fish",
          "pic" -> "Pic of size: 100",
          "future" -> Map("a" -> "Apple"),
          "deep" -> Map(
            "a" -> "Already Been Done",
            "b" -> "Boring",
            "c" -> List("Contrived", null, "Confusing"),
            "deeper" -> List(
              Map("a" -> "Apple", "b" -> "Banana"),
              null,
              Map("a" -> "Apple", "b" -> "Banana")
            )
          )
        )
      )

      val schema = Schema(DataType)

      Executor
        .execute(schema, doc, Ctx(), new TestSubject, variables = mapVars(Map("size" -> 100)))
        .await should be(expected)
    }

    "prepare and execute arbitrary queries" in {
      val doc = gql"""
        query Example($$size: Int) {
          a,
          b,
          x: c
          ...c
          f
          ...on DataType {
            pic(size: $$size)
            future {
              a
            }
          }
          deep {
            a
            b
            c
            deeper {
              a
              b
            }
          }
        }

        fragment c on DataType {
          d
          e
        }
      """

      val expected = Map(
        "data" -> Map(
          "a" -> "Apple",
          "b" -> "Banana",
          "x" -> "Cookie",
          "d" -> "Donut",
          "e" -> "Egg",
          "f" -> "Fish",
          "pic" -> "Pic of size: 100",
          "future" -> Map("a" -> "Apple"),
          "deep" -> Map(
            "a" -> "Already Been Done",
            "b" -> "Boring",
            "c" -> List("Contrived", null, "Confusing"),
            "deeper" -> List(
              Map("a" -> "Apple", "b" -> "Banana"),
              null,
              Map("a" -> "Apple", "b" -> "Banana")
            )
          )
        )
      )

      val schema = Schema(DataType)

      val preparedQuery = Executor
        .prepare(schema, doc, Ctx(), new TestSubject, variables = mapVars(Map("size" -> 100)))
        .await

      preparedQuery.execute().await should be(expected)
    }

    "[regression] execute queries with reducers that use variables" in {
      val doc = gql"""
        query Example($$size: Int) {
          ...on DataType {
            pic(size: $$size)
          }
        }
      """

      val expected = Map("data" -> Map("pic" -> "Pic of size: 100"))

      val schema = Schema(DataType)

      var sizeValue: Int = -1

      object PicSizeFinderReducer extends QueryReducer[Ctx, Ctx] {
        type Acc = Unit
        def initial: Acc = ()
        def reduceAlternatives(alternatives: Seq[Acc]): Acc = initial

        def reduceField[Val](
            fieldAcc: Acc,
            childrenAcc: Acc,
            path: ExecutionPath,
            ctx: Ctx,
            astFields: Vector[ast.Field],
            parentType: ObjectType[Ctx, Val],
            field: Field[Ctx, Val],
            argumentValuesFn: QueryReducer.ArgumentValuesFn): Acc = {
          val Success(args) = argumentValuesFn(path, field.arguments, astFields.head.arguments)
          args.argOpt[Int]("size").foreach { picSize =>
            sizeValue = picSize
          }
        }

        def reduceScalar[T](path: ExecutionPath, ctx: Ctx, tpe: ScalarType[T]): Acc = initial
        def reduceEnum[T](path: ExecutionPath, ctx: Ctx, tpe: EnumType[T]): Acc = initial
        def reduceCtx(acc: Acc, ctx: Ctx): ReduceAction[Ctx, Ctx] = Value(ctx)
      }

      Executor
        .execute(
          schema,
          doc,
          Ctx(),
          new TestSubject,
          variables = mapVars(Map("size" -> 100)),
          queryReducers = PicSizeFinderReducer :: Nil)
        .await should be(expected)
      sizeValue should be(100)
    }

    "prepare should validate in the preparation stage" in {
      val doc = gql"""
        query Example($$size: String) {
          ...on DataType {
            pic(size: $$size)
          }
        }
      """

      val schema = Schema(DataType)

      an[ValidationError] should be thrownBy
        Executor
          .prepare(schema, doc, Ctx(), new TestSubject, variables = mapVars(Map("size" -> 100)))
          .await
    }

    "prepare should execute query reducers in the preparation stage" in {
      val doc = gql"""
        {
          __schema {
            queryType {name}
          }
        }
      """

      val schema = Schema(DataType)

      val introQR = QueryReducer.hasIntrospection[Ctx]((hasIntro, ctx) =>
        ctx.copy(color = if (hasIntro) "red" else "blue"))
      val failQR = QueryReducer.hasIntrospection[Ctx]((hasIntro, ctx) =>
        if (hasIntro) throw new IllegalStateException("foo") else ctx)

      an[QueryReducingError] should be thrownBy
        Executor.prepare(schema, doc, Ctx(), new TestSubject, queryReducers = failQR :: Nil).await

      val prepared =
        Executor.prepare(schema, doc, Ctx(), new TestSubject, queryReducers = introQR :: Nil).await

      prepared.userContext.color should be("red")
    }

    "respect max depth level" in {
      val Success(doc) = QueryParser.parse("""
        query Foo {
          d1: deep {
            deep {
              deep {
                deep {
                  deep {
                    a
                    deep {
                      b
                    }
                  }
                }
              }
            }
          }
          d2: deep {
            deep {
              deep {
                deep {
                  deep {
                    a
                    deep {
                      b
                      d
                      c
                    }
                  }
                }
              }
            }
          }
        }
        """)

      val expected = Map(
        "data" -> Map(
          "d1" -> Map(
            "deep" -> Map(
              "deep" -> Map(
                "deep" -> Map(
                  "deep" -> Map(
                    "a" -> "Apple",
                    "deep" -> Map(
                      "b" -> null
                    )
                  )
                )
              )
            )
          ),
          "d2" -> Map(
            "deep" -> Map(
              "deep" -> Map(
                "deep" -> Map(
                  "deep" -> Map(
                    "a" -> "Apple",
                    "deep" -> null
                  )
                )
              )
            )
          )
        ),
        "errors" -> List(
          Map(
            "message" -> "Max query depth 6 is reached.",
            "path" -> List("d1", "deep", "deep", "deep", "deep", "deep", "b"),
            "locations" -> List(Map("line" -> 10, "column" -> 23))
          ),
          Map(
            "message" -> "Max query depth 6 is reached.",
            "path" -> List("d2", "deep", "deep", "deep", "deep", "deep", "b"),
            "locations" -> List(Map("line" -> 24, "column" -> 23))
          ),
          Map(
            "message" -> "Max query depth 6 is reached.",
            "path" -> List("d2", "deep", "deep", "deep", "deep", "deep", "d"),
            "locations" -> List(Map("line" -> 25, "column" -> 23))
          )
        )
      )

      val schema = Schema(ParallelFragmentType)

      Executor.execute(schema, doc, maxQueryDepth = Some(6)).await should be(expected)
    }

    "merge parallel fragments" in {
      val schema = Schema(ParallelFragmentType)

      val Success(doc) = QueryParser.parse("""
        { a, ...FragOne, ...FragTwo }

        fragment FragOne on Type {
          b
          deep { b, deeper: deep { b } }
        }

        fragment FragTwo on Type {
          c
          deep { c, deeper: deep { c } }
        }
      """)

      val expected = Map(
        "data" -> Map(
          "a" -> "Apple",
          "b" -> "Banana",
          "c" -> "Cherry",
          "deep" -> Map(
            "b" -> "Banana",
            "c" -> "Cherry",
            "deeper" -> Map("b" -> "Banana", "c" -> "Cherry")))
      )

      Executor.execute(schema, doc).await should be(expected)
    }

    "threads context correctly" in {
      case class Thing(a: Option[String])

      var resolvedCtx: Option[String] = None

      val schema = Schema(
        ObjectType(
          "Type",
          fields[Unit, Thing](
            Field(
              "a",
              OptionType(StringType),
              resolve = ctx => { resolvedCtx = ctx.value.a; ctx.value.a }))))

      val Success(doc) = QueryParser.parse("query Example { a }")

      Executor.execute(schema, doc, root = Thing(Some("thing"))).await should be(
        Map("data" -> Map("a" -> "thing")))

      resolvedCtx should be(Some("thing"))
    }

    "correctly threads arguments" in {
      var resolvedArgs: Map[String, Any] = Map.empty

      val schema = Schema(
        ObjectType(
          "Type",
          fields[Unit, Unit](Field(
            "b",
            OptionType(StringType),
            arguments = Argument[Option[Int @@ CoercedScalaResult]](
              "numArg",
              OptionInputType(IntType)) :: Argument[Option[String @@ CoercedScalaResult]](
              "stringArg",
              OptionInputType(StringType)) :: Nil,
            resolve = ctx => {
              resolvedArgs = ctx.args.raw; None
            }
          ))
        ))

      val Success(doc) = QueryParser.parse("""
        query Example {
          b(numArg: 123, stringArg: "foo")
        }
      """)

      Executor.execute(schema, doc).await
      resolvedArgs should be(Map("numArg" -> Some(123), "stringArg" -> Some("foo")))
    }

    "null out error subtrees" in {
      class Data {
        def sync = "sync"
        def syncError = throw new IllegalStateException("Error getting syncError")
        def async = Future.successful("async")
        def asyncReject: Future[String] =
          Future.failed(new IllegalStateException("Error getting asyncReject"))
        def asyncError: Future[String] = Future {
          throw new IllegalStateException("Error getting asyncError")
        }
      }

      val schema =
        Schema(
          ObjectType(
            "Type",
            fields[Unit, Data](
              Field("sync", OptionType(StringType), resolve = _.value.sync),
              Field("syncError", OptionType(StringType), resolve = _.value.syncError),
              Field("async", OptionType(StringType), resolve = _.value.async),
              Field("asyncReject", OptionType(StringType), resolve = ctx => ctx.value.asyncReject),
              Field("asyncError", OptionType(StringType), resolve = _.value.asyncError),
              Field(
                "syncDeferError",
                OptionType(StringType),
                resolve = ctx =>
                  DeferredValue(throw new IllegalStateException("Error getting syncDeferError"))),
              Field(
                "asyncDeferError",
                OptionType(StringType),
                resolve = _ =>
                  DeferredFutureValue(
                    Future.failed(throw new IllegalStateException("Error getting asyncDeferError")))
              )
            )
          ))

      val Success(doc) = QueryParser.parse("""
        {
          sync,
             syncError,
           async,
          asyncReject,
           asyncDeferError
              asyncError
              syncDeferError
        }""")

      val exceptionHandler = ExceptionHandler { case (m, e: IllegalStateException) =>
        HandledException(e.getMessage)
      }

      val result = Executor
        .execute(schema, doc, root = new Data, exceptionHandler = exceptionHandler)
        .await
        .asInstanceOf[Map[String, Any]]

      val data = result("data")
      val errors = result("errors").asInstanceOf[Seq[_]]

      data should be(
        Map(
          "sync" -> "sync",
          "syncError" -> null,
          "async" -> "async",
          "asyncReject" -> null,
          "asyncError" -> null,
          "asyncDeferError" -> null,
          "syncDeferError" -> null
        ))

      errors should (have(size(5))
        .and(
          contain(
            Map(
              "path" -> List("syncError"),
              "locations" -> List(Map("line" -> 4, "column" -> 14)),
              "message" -> "Error getting syncError")))
        .and(
          contain(
            Map(
              "path" -> List("asyncReject"),
              "locations" -> List(Map("line" -> 6, "column" -> 11)),
              "message" -> "Error getting asyncReject")))
        .and(
          contain(
            Map(
              "message" -> "Error getting asyncDeferError",
              "path" -> List("asyncDeferError"),
              "locations" -> List(Map("line" -> 7, "column" -> 12)))))
        .and(
          contain(
            Map(
              "message" -> "Error getting syncDeferError",
              "path" -> List("syncDeferError"),
              "locations" -> List(Map("line" -> 9, "column" -> 15)))))
        .and(
          contain(
            Map(
              "path" -> List("asyncError"),
              "locations" -> List(Map("line" -> 8, "column" -> 15)),
              "message" -> "Error getting asyncError"))))
    }

    "use the inline operation if no operation is provided" in {
      val schema = Schema(
        ObjectType(
          "Type",
          fields[Unit, Unit](Field("a", OptionType(StringType), resolve = _ => "b"))))
      val Success(doc) = QueryParser.parse("{ a }")

      Executor.execute(schema, doc).await should be(Map("data" -> Map("a" -> "b")))
    }

    "use the only operation if no operation is provided" in {
      val schema = Schema(
        ObjectType(
          "Type",
          fields[Unit, Unit](Field("a", OptionType(StringType), resolve = _ => "b"))))
      val Success(doc) = QueryParser.parse("query Example { a }")

      Executor.execute(schema, doc).await should be(Map("data" -> Map("a" -> "b")))
    }

    "throw if no operation is provided with multiple operations" in {
      val schema = Schema(
        ObjectType(
          "Type",
          fields[Unit, Unit](Field("a", OptionType(StringType), resolve = _ => "b"))))
      val Success(doc) = QueryParser.parse("query Example { a } query OtherExample { a }")

      val error = intercept[OperationSelectionError](Executor.execute(schema, doc).await)

      error.getMessage should be(
        "Must provide operation name if query contains multiple operations")
    }

    "throw if the operation name is invalid" in {
      val schema = Schema(
        ObjectType(
          "Type",
          fields[Unit, Unit](Field("a", OptionType(StringType), resolve = _ => "b"))))
      val Success(doc) = QueryParser.parse("query Example { a }")

      val error = intercept[OperationSelectionError](
        Executor.execute(schema, doc, operationName = Some("Eggsample")).await)

      error.getMessage should be("Unknown operation name 'Eggsample'")
    }

    "use correct schema type schema for operation" in {
      val schema = Schema(
        ObjectType("Q", fields[Unit, Unit](Field("a", OptionType(StringType), resolve = _ => "b"))),
        Some(
          ObjectType(
            "M",
            fields[Unit, Unit](Field("c", OptionType(StringType), resolve = _ => "d")))),
        Some(
          ObjectType(
            "S",
            fields[Unit, Unit](Field("e", OptionType(StringType), resolve = _ => "f"))))
      )

      val Success(doc) = QueryParser.parse("query Q { a } mutation M { c } subscription S { e }")

      Executor.execute(schema, doc, operationName = Some("Q")).await should be(
        Map("data" -> Map("a" -> "b")))
      Executor.execute(schema, doc, operationName = Some("M")).await should be(
        Map("data" -> Map("c" -> "d")))
      Executor.execute(schema, doc, operationName = Some("S")).await should be(
        Map("data" -> Map("e" -> "f")))
    }

    "correct field ordering despite execution order" in {
      case class Sum(a: Int, b: Int) extends Deferred[Int]

      class MyResolver extends DeferredResolver[Any] {
        def resolve(deferred: Vector[Deferred[Any]], ctx: Any, queryState: Any)(implicit
            ec: ExecutionContext) = deferred.map { case Sum(a, b) =>
          Future(a + b)(ec)
        }
      }

      val schema = Schema(
        ObjectType(
          "Type",
          fields[Unit, Unit](
            Field("c", OptionType(StringType), resolve = _ => "c"),
            Field("a", OptionType(StringType), resolve = _ => Future { Thread.sleep(30); "a" }),
            Field("d", OptionType(StringType), resolve = _ => Future { Thread.sleep(5); "d" }),
            Field("b", OptionType(IntType), resolve = _ => Sum(1, 2)),
            Field("e", OptionType(StringType), resolve = _ => "e")
          )
        ))

      def keys(res: Any) =
        res.asInstanceOf[Map[String, Any]]("data").asInstanceOf[Map[String, Any]].keys.toList

      keys(
        Executor
          .execute(schema, graphql"query Q { a b c d e }", deferredResolver = new MyResolver)
          .await) should be(List("a", "b", "c", "d", "e"))

      keys(
        Executor
          .execute(schema, graphql"query Q { e, d, c, b, a }", deferredResolver = new MyResolver)
          .await) should be(List("e", "d", "c", "b", "a"))

      keys(
        Executor
          .execute(schema, graphql"query Q { e, a, d, c, b }", deferredResolver = new MyResolver)
          .await) should be(List("e", "a", "d", "c", "b"))
    }

    "correct field ordering despite execution order (fragments & directives)" in {
      case class Sum(a: Int, b: Int) extends Deferred[Int]

      class MyResolver extends DeferredResolver[Any] {
        def resolve(deferred: Vector[Deferred[Any]], ctx: Any, queryState: Any)(implicit
            ec: ExecutionContext) = deferred.map { case Sum(a, b) =>
          Future(a + b)(ec)
        }
      }

      val schema = Schema(
        ObjectType(
          "Query",
          fields[Unit, Unit](
            Field("qux", OptionType(StringType), resolve = _ => "c"),
            Field("bar", OptionType(StringType), resolve = _ => Future { Thread.sleep(30); "a" }),
            Field("foo", OptionType(StringType), resolve = _ => Future { Thread.sleep(5); "d" }),
            Field("baz", OptionType(IntType), resolve = _ => Sum(1, 2))
          )
        ))

      import sangria.marshalling.queryAst._
      import sangria.ast

      def keys(res: ast.Value) =
        res
          .asInstanceOf[ast.ObjectValue]
          .fieldsByName("data")
          .asInstanceOf[ast.ObjectValue]
          .fields
          .map(_.name)

      val withFragment =
        graphql"""
          {
            foo
            ...Frag
            qux
          }

          fragment Frag on Query {
            bar
            baz
          }
        """

      keys(
        Executor.execute(schema, withFragment, deferredResolver = new MyResolver).await) should be(
        List("foo", "bar", "baz", "qux"))

      val withComplexFragment =
        graphql"""
          {
            foo
            ...Matching
            bar
          }

          fragment Matching on Query {
            bar
            qux
            foo
          }
        """

      keys(
        Executor
          .execute(schema, withComplexFragment, deferredResolver = new MyResolver)
          .await) should be(List("foo", "bar", "qux"))

      val withDirectives =
        graphql"""
          {
            foo @skip(if: true)
            bar
            foo
          }
        """

      keys(
        Executor
          .execute(schema, withDirectives, deferredResolver = new MyResolver)
          .await) should be(List("bar", "foo"))

    }

    "avoid recursion" in {
      val schema = Schema(
        ObjectType(
          "Type",
          fields[Unit, Unit](Field("a", OptionType(StringType), resolve = _ => "b"))))

      val Success(doc) = QueryParser.parse("""
        query Q {
          a
          ...Frag
          ...Frag
        }

        fragment Frag on Type {
          a,
          ...Frag
        }
      """)

      Executor
        .execute(schema, doc, operationName = Some("Q"), queryValidator = QueryValidator.empty)
        .await should be(Map("data" -> Map("a" -> "b")))
    }

    "not include illegal fields in output" in {
      val schema = Schema(
        ObjectType("Q", fields[Unit, Unit](Field("a", OptionType(StringType), resolve = _ => "b"))),
        Some(
          ObjectType(
            "M",
            fields[Unit, Unit](Field("c", OptionType(StringType), resolve = _ => "d"))))
      )
      val Success(doc) = QueryParser.parse("mutation M { thisIsIllegalDontIncludeMe }")

      Executor.execute(schema, doc, queryValidator = QueryValidator.empty).await should be(
        Map("data" -> Map()))
    }

    "update context in query operations" in {
      val Success(doc) = QueryParser.parse("""
        query Q {
          ctxUpdating {
            ctxColor
          }

          ctxUpdatingFut {
            ctxColor
          }

          ctxUpdatingDef {
            ctxColor
          }

          ctxUpdatingDefFut {
            ctxColor
          }
        }
        """)

      val schema = Schema(DataType)

      Executor
        .execute(schema, doc, Ctx(), new TestSubject, deferredResolver = new LightColorResolver)
        .await should be(
        Map("data" -> Map(
          "ctxUpdating" -> Map("ctxColor" -> "blue"),
          "ctxUpdatingFut" -> Map("ctxColor" -> "orange"),
          "ctxUpdatingDef" -> Map("ctxColor" -> "lightmagenta"),
          "ctxUpdatingDefFut" -> Map("ctxColor" -> "lightred")
        )))
    }

    "resolve deferred values correctly" in {
      val query = graphql"""
        {
          def { color }
          defFut { color }
        }
        """

      val schema = Schema(DataType)

      Executor
        .execute(
          schema,
          query,
          root = new TestSubject,
          userContext = Ctx(),
          deferredResolver = new LightColorResolver)
        .await should be(
        Map(
          "data" -> Map(
            "def" -> Map("color" -> "lightmagenta"),
            "defFut" -> Map("color" -> "lightred"))))
    }

    "ensure that deferred resolver complied to the contract" in {
      val query = graphql"""
        {
          def { color }
        }
        """

      val schema = Schema(DataType)

      val exceptionHandler = ExceptionHandler { case (m, e: IllegalStateException) =>
        HandledException(e.getMessage)
      }

      val res = Executor
        .execute(
          schema,
          query,
          root = new TestSubject,
          userContext = Ctx(),
          deferredResolver = new BrokenLightColorResolver,
          exceptionHandler = exceptionHandler)
        .await
        .asInstanceOf[Map[String, Any]]

      res("data") should be(null: Any)
      res("errors").asInstanceOf[Seq[Map[String, Any]]](0)("message") should be(
        "Deferred resolver returned 2 elements, but it got 1 deferred values. This violates the contract. You can find more information in the documentation: https://sangria-graphql.github.io/learn/#deferred-values-and-resolver")
    }

    "resolve deferred values correctly in presence of errors" in {
      val Success(doc) = QueryParser.parse("""
        {
          defFail { color }
          defFutFail { color }
        }
        """)

      val schema = Schema(DataType)

      val exceptionHandler = ExceptionHandler { case (m, e: IllegalStateException) =>
        HandledException(e.getMessage)
      }

      Executor
        .execute(
          schema,
          doc,
          root = new TestSubject,
          userContext = Ctx(),
          deferredResolver = new LightColorResolver,
          exceptionHandler = exceptionHandler)
        .await should be(
        Map(
          "data" -> Map("defFail" -> null, "defFutFail" -> null),
          "errors" -> List(
            Map(
              "message" -> "error in resolver",
              "path" -> List("defFail"),
              "locations" -> List(Map("line" -> 3, "column" -> 11))),
            Map(
              "message" -> "error in resolver",
              "path" -> List("defFutFail"),
              "locations" -> List(Map("line" -> 4, "column" -> 11)))
          )
        ))
    }

    "fails to execute a query containing a type definition" in {
      val Success(doc) = QueryParser.parse("""
          { a }

          type Query { foo: String }
        """)

      val schema = Schema(DataType)

      val error = intercept[ExecutionError] {
        Executor
          .execute(
            schema,
            doc,
            root = new TestSubject,
            userContext = Ctx(),
            queryValidator = QueryValidator.empty)
          .await
      }

      error.getMessage should be(
        "GraphQL cannot execute a request containing a ObjectTypeDefinition.")
    }

    "handles partial values" in {
      val Success(doc) = QueryParser.parse("{eager, future}")

      case class MyListError(message: String) extends Exception(message)

      val QueryType = ObjectType(
        "Query",
        fields[Unit, Unit](
          Field(
            "eager",
            ListType(StringType),
            resolve = _ =>
              PartialValue(List("a", "c"), Vector(MyListError("error 1"), MyListError("error 2")))),
          Field(
            "future",
            ListType(StringType),
            resolve = _ =>
              PartialFutureValue(
                Future.successful(
                  PartialValue[Unit, List[String]](
                    List("d", "f"),
                    Vector(MyListError("error 3"), MyListError("error 4")))))
          )
        )
      )

      val schema = Schema(QueryType)

      val exceptionHandler = ExceptionHandler { case (m, e: MyListError) =>
        HandledException(e.getMessage)
      }

      val result = Executor
        .execute(schema, doc, exceptionHandler = exceptionHandler)
        .await
        .asInstanceOf[Map[String, Any]]

      result("data") should be(Map("eager" -> Vector("a", "c"), "future" -> Vector("d", "f")))

      val errors = result("errors").asInstanceOf[Seq[Any]]

      errors should (have(size(4))
        .and(
          contain(
            Map(
              "message" -> "error 1",
              "path" -> List("eager"),
              "locations" -> Vector(Map("line" -> 1, "column" -> 2)))))
        .and(
          contain(
            Map(
              "message" -> "error 2",
              "path" -> List("eager"),
              "locations" -> Vector(Map("line" -> 1, "column" -> 2)))))
        .and(
          contain(
            Map(
              "message" -> "error 3",
              "path" -> List("future"),
              "locations" -> Vector(Map("line" -> 1, "column" -> 9)))))
        .and(
          contain(
            Map(
              "message" -> "error 4",
              "path" -> List("future"),
              "locations" -> Vector(Map("line" -> 1, "column" -> 9))))))
    }

    "handles Iterables as ListType" in {
      val Success(doc) = QueryParser.parse("{set}")

      val QueryType = ObjectType(
        "Query",
        fields[Unit, Unit](
          Field("set", ListType(StringType), resolve = _ => Set("one", "two", "three"))
        )
      )

      val schema = Schema(QueryType)

      val result = Executor
        .execute(schema, doc)
        .await
        .asInstanceOf[Map[String, Any]]

      result("data") should be(Map("set" -> Vector("one", "two", "three")))
    }

    "support extended result in queries" in {
      import ExecutionScheme.Extended

      case class MyCtx(complexity: Double)

      case class MyListError(message: String) extends Exception(message)

      val QueryType = ObjectType(
        "Query",
        fields[MyCtx, Unit](
          Field("hello", StringType, complexity = Some((_, _, _) => 123), resolve = _ => "world"),
          Field(
            "error",
            OptionType(StringType),
            resolve = _ => throw new IllegalStateException("foo"))
        )
      )

      val schema = Schema(QueryType)

      val reducer = QueryReducer.measureComplexity[MyCtx]((c, ctx) => ctx.copy(complexity = c))

      val exceptionHandler = ExceptionHandler { case (m, e: IllegalStateException) =>
        HandledException(e.getMessage)
      }

      val middleware = new Middleware[MyCtx] {
        type QueryVal = Int
        def beforeQuery(context: MiddlewareQueryContext[MyCtx, _, _]) = 345
        def afterQuery(queryVal: QueryVal, context: MiddlewareQueryContext[MyCtx, _, _]) = ()
      }

      val ctx = MyCtx(0)

      val result = Executor
        .execute(
          schema,
          graphql"{h1: hello, h2: hello, error}",
          ctx,
          exceptionHandler = exceptionHandler,
          middleware = middleware :: Nil,
          queryReducers = reducer :: Nil)
        .await

      result.result.asInstanceOf[Map[String, Any]]("data") should be(
        Map("h1" -> "world", "h2" -> "world", "error" -> null))

      result.errors should have size 1
      result.errors(0).error.getMessage should be("foo")

      result.ctx.complexity should be(247)
      result.middlewareVals(0)._1 should be(345)
    }

    "support extended result in mutations" in {
      import ExecutionScheme.Extended

      case class MyCtx(acc: String)

      case class MyListError(message: String) extends Exception(message)

      val QueryType =
        ObjectType("Query", fields[MyCtx, Unit](Field("hello", StringType, resolve = _ => "world")))

      val MutationType = ObjectType(
        "Mutation",
        fields[MyCtx, Unit](
          Field(
            "add",
            StringType,
            arguments = Argument("str", StringType) :: Nil,
            resolve = c => UpdateCtx(Future(c.ctx.acc + c.arg[String]("str")))(v => c.ctx.copy(v))))
      )

      val schema = Schema(QueryType, Some(MutationType))

      val ctx = MyCtx("")

      val query =
        graphql"""
          mutation {
            a1: add(str: "One")
            a2: add(str: "Two")
            a3: add(str: "Three")
          }
        """

      val result = Executor.execute(schema, query, ctx).await

      result.result.asInstanceOf[Map[String, Any]]("data") should be(
        Map("a1" -> "One", "a2" -> "OneTwo", "a3" -> "OneTwoThree"))

      result.errors should have size 0

      result.ctx.acc should be("OneTwoThree")
    }

    "validate recursive fragments" in checkContainsErrors(
      schema = Schema(DataType),
      data = (),
      userContext = Ctx(),
      query = """
          {
            ...c
          }

          fragment one on DataType {
            pic(size: 1)
          }

          fragment two on DataType {
            pic(size: 3)
          }

          fragment c on DataType {
            ...c
            ...one
            ...two
          }
        """,
      expectedData = None,
      expectedErrorStrings = List(
        "Cannot spread fragment 'c' within itself." -> List(Pos(15, 13)),
        "Conflict at 'pic' because of differing arguments. Use different aliases on the fields to fetch both if this was intentional." -> List(
          Pos(7, 13),
          Pos(11, 13))
      )
    )

    "validate mutually recursive fragments" in checkContainsErrors(
      schema = Schema(DataType),
      data = (),
      userContext = Ctx(),
      query = """
          {
            ...c
          }

          fragment one on DataType {
            pic(size: 1)
          }

          fragment two on DataType {
            pic(size: 3)
          }

          fragment c on DataType {
            ...d
          }

          fragment d on DataType {
            ...c
            ...one
            ...two
          }
        """,
      expectedData = None,
      expectedErrorStrings = List(
        "Cannot spread fragment 'c' within itself via 'd'." -> List(Pos(15, 13), Pos(19, 13)),
        "Conflict at 'pic' because of differing arguments. Use different aliases on the fields to fetch both if this was intentional." -> List(
          Pos(7, 13),
          Pos(11, 13))
      )
    )

    "support `Action.sequence` in queries and mutations" in {
      val error = new IllegalStateException("foo")

      val fetcher =
        Fetcher[Unit, Int, Int]((_, ids) => Future.successful(ids.map(_ + 100)))(HasId(_ - 100))

      lazy val QueryType = ObjectType(
        "Query",
        fields[Unit, Unit](
          Field(
            "ids",
            OptionType(ListType(OptionType(IntType))),
            resolve = c =>
              Action
                .sequence(
                  Seq(
                    LeafAction(Some(1)),
                    LeafAction(None),
                    LeafAction(Some(2)),
                    LeafAction(Success(Some(3))),
                    LeafAction(Future(Some(4))),
                    LeafAction(PartialValue(Some(5), Vector(error))),
                    PartialFutureValue[Unit, Option[Int]](
                      Future(PartialValue(Some(6), Vector(error)))),
                    LeafAction(fetcher.deferOpt(7)),
                    LeafAction(fetcher.deferOpt(8)),
                    LeafAction(Future(fetcher.deferOpt(9))),
                    LeafAction(Future(fetcher.deferOpt(10)))
                  ).map(_.map(_.map(_ + 10))))
                .map(vs => vs.map(_.map(_ + 1)))
          )
        )
      )

      val schema = Schema(QueryType, Some(QueryType))

      val exceptionHandler = ExceptionHandler { case (m, e: IllegalStateException) =>
        HandledException(e.getMessage)
      }

      val queryRes = Executor.execute(
        schema,
        graphql"query {ids, foo: ids}",
        exceptionHandler = exceptionHandler,
        deferredResolver = DeferredResolver.fetchers(fetcher))

      val mutationRes = Executor.execute(
        schema,
        graphql"mutation {ids, foo: ids}",
        exceptionHandler = exceptionHandler,
        deferredResolver = DeferredResolver.fetchers(fetcher))

      Seq(queryRes.await -> 8, mutationRes.await -> 11).foreach { case (result, offset) =>
        result should be(
          Map(
            "data" -> Map(
              "ids" -> Vector(12, null, 13, 14, 15, 16, 17, 118, 119, 120, 121),
              "foo" -> Vector(12, null, 13, 14, 15, 16, 17, 118, 119, 120, 121)),
            "errors" -> Vector(
              Map(
                "message" -> "foo",
                "path" -> Vector("ids"),
                "locations" -> Vector(Map("line" -> 1, "column" -> offset))),
              Map(
                "message" -> "foo",
                "path" -> Vector("ids"),
                "locations" -> Vector(Map("line" -> 1, "column" -> offset))),
              Map(
                "message" -> "foo",
                "path" -> Vector("foo"),
                "locations" -> Vector(Map("line" -> 1, "column" -> (5 + offset)))),
              Map(
                "message" -> "foo",
                "path" -> Vector("foo"),
                "locations" -> Vector(Map("line" -> 1, "column" -> (5 + offset))))
            )
          ))
      }
    }
  }
}
