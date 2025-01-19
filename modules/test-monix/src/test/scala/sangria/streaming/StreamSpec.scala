package sangria.streaming

import language.postfixOps
import sangria.execution.deferred.{DeferredResolver, Fetcher, HasId}
import sangria.execution.{ExceptionHandler, Executor, HandledException}
import sangria.util.FutureResultSupport
import sangria.schema._
import sangria.macros._
import sangria.macros.derive._
import sangria.validation.QueryValidator
import sangria.validation.rules.SingleFieldSubscriptions

import scala.concurrent.duration._
import scala.concurrent.Future
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class StreamSpec extends AnyWordSpec with Matchers with FutureResultSupport {
  val timeout = 10.seconds

  case class Fruit(id: Int, name: String, color: String)

  case class FruitEaten(name: String, eater: String)
  case class FruitSmashed(id: Int)

  trait Mutation {
    this: Ctx =>

    @GraphQLField
    def eatFruit(name: String, eater: String): String = {
      eventBus.onNext(FruitEaten(name, eater))

      "OmNomNom"
    }

    @GraphQLField
    def smashFruit(id: Int) = {
      eventBus.onNext(FruitSmashed(id))

      "Splash!"
    }

    @GraphQLField
    def stop = {
      eventBus.onComplete()

      "Full!"
    }
  }
  class Ctx extends Mutation {
    val eventBus = _root_.monix.reactive.subjects.ReplaySubject[Any]()
  }

  "Stream based subscriptions" when {
    "using monix" should {
      import _root_.monix.execution.Scheduler.Implicits.global
      import _root_.monix.reactive.Observable
      import _root_.monix.reactive.subjects.ReplaySubject

      import sangria.marshalling.sprayJson._
      import spray.json._

      import sangria.streaming.monix._

      val QueryType = ObjectType(
        "QueryType",
        fields[Unit, Unit](Field("hello", StringType, resolve = _ => "world")))

      "Stream results with monix" in {
        val SubscriptionType = ObjectType(
          "Subscription",
          fields[Unit, Unit](
            Field.subs("letters", StringType, resolve = _ => Observable("a", "b").map(Action(_))),
            Field.subs(
              "numbers",
              OptionType(IntType),
              resolve = _ => Observable(1, 2).map(Action(_)))
          )
        )

        val schema = Schema(QueryType, subscription = Some(SubscriptionType))

        import sangria.execution.ExecutionScheme.Stream

        val stream: Observable[JsValue] =
          Executor.execute(
            schema,
            graphql"subscription { letters numbers }",
            queryValidator = QueryValidator.default.withoutValidation[SingleFieldSubscriptions])

        val result = stream.toListL.runToFuture.await(timeout)

        result should (have(size(4))
          .and(contain("""{"data": {"letters": "a"}}""".parseJson))
          .and(contain("""{"data": {"letters": "b"}}""".parseJson))
          .and(contain("""{"data": {"numbers": 1}}""".parseJson))
          .and(contain("""{"data": {"numbers": 2}}""".parseJson)))
      }

      "recover stream errors" in {
        val SubscriptionType = ObjectType(
          "Subscription",
          fields[Unit, Unit](
            Field.subs(
              "letters",
              OptionType(StringType),
              resolve = _ =>
                Observable("a", "b", "c", "d", "e")
                  .map { l =>
                    if (l == "c") throw new IllegalStateException("foo")
                    else l
                  }
                  .map(Action(_))
            ),
            Field.subs(
              "numbers",
              OptionType(IntType),
              resolve = _ => Observable(1, 2, 3, 4).map(Action(_)))
          )
        )

        val schema = Schema(QueryType, subscription = Some(SubscriptionType))

        import sangria.execution.ExecutionScheme.Stream

        val exceptionHandler = ExceptionHandler { case (m, e: IllegalStateException) =>
          HandledException(e.getMessage)
        }

        val stream: Observable[JsValue] =
          Executor.execute(
            schema,
            graphql"subscription { letters numbers }",
            queryValidator = QueryValidator.default.withoutValidation[SingleFieldSubscriptions],
            exceptionHandler = exceptionHandler
          )

        val result = stream.toListL.runToFuture.await(timeout)

        result should (contain(
          """{"data": {"letters": null}, "errors": [{"message": "foo", "path":["letters"]}]}""".parseJson)
          .and(contain("""{"data": {"numbers": 1}}""".parseJson))
          .and(contain("""{"data": {"numbers": 2}}""".parseJson))
          .and(contain("""{"data": {"numbers": 3}}""".parseJson))
          .and(contain("""{"data": {"numbers": 4}}""".parseJson)))
      }

      "complex stream scenario" in {
        val cherryPicker = Fetcher.caching[Ctx, Fruit, Int]((ctx, ids) =>
          Future.successful(ids.map(id => Fruit(id, "cherry", "red"))))(HasId(_.id))

        val FruitType = ObjectType(
          "Fruit",
          fields[Unit, Fruit](
            Field("name", StringType, resolve = _.value.name),
            Field("color", StringType, resolve = _.value.color)
          ))

        val FruitEatenType = ObjectType(
          "FruitEaten",
          fields[Unit, FruitEaten](
            Field("name", StringType, resolve = _.value.name),
            Field("eater", StringType, resolve = _.value.eater)
          ))

        val FruitSmashedType = ObjectType(
          "FruitSmashed",
          fields[Unit, FruitSmashed](
            Field("fruit", FruitType, resolve = c => cherryPicker.defer(c.value.id))
          ))

        val FruitEventType =
          UnionType("FruitEvent", types = FruitEatenType :: FruitSmashedType :: Nil)

        val QueryType = ObjectType(
          "QueryType",
          fields[Ctx, Unit](Field("hello", StringType, resolve = _ => "world")))

        val MutationType = deriveContextObjectType[Ctx, Mutation, Unit](identity)

        val SubscriptionType = ObjectType(
          "Subscription",
          fields[Ctx, Unit](
            Field.subs(
              "fruitEvents",
              OptionType(FruitEventType),
              resolve = c => c.ctx.eventBus.map(Action(_)))
          ))

        val schema = Schema(QueryType, Some(MutationType), Some(SubscriptionType))

        import sangria.execution.ExecutionScheme.Stream

        val exceptionHandler = ExceptionHandler { case (m, e: IllegalStateException) =>
          HandledException(e.getMessage)
        }

        val subscription =
          graphql"""
            subscription {
              fruitEvents {
                ... on FruitEaten {name, eater}
                ... on FruitSmashed {
                  fruit {
                    name
                    color
                  }
                }
              }
            }
          """

        val ctx = new Ctx

        val stream: Observable[JsValue] =
          Executor.execute(
            schema,
            subscription,
            ctx,
            deferredResolver = DeferredResolver.fetchers(cherryPicker),
            exceptionHandler = exceptionHandler)

        val mutation =
          graphql"""
            mutation {
               e1: eatFruit(name: "banana", eater: "me")
               smashFruit(id: 123)
               e2: eatFruit(name: "orange", eater: "someone else")
               stop
            }
          """

        {
          import sangria.execution.ExecutionScheme.Default

          Executor
            .execute(
              schema,
              mutation,
              ctx,
              deferredResolver = DeferredResolver.fetchers(cherryPicker),
              exceptionHandler = exceptionHandler)
            .await(timeout)
        }

        val result = stream.toListL.runToFuture.await(timeout)

        result should (have(size(3))
          .and(
            contain("""{"data": {"fruitEvents": {"name": "banana", "eater": "me"}}}""".parseJson))
          .and(contain(
            """{"data": {"fruitEvents": {"name": "orange", "eater": "someone else"}}}""".parseJson))
          .and(contain(
            """{"data": {"fruitEvents": {"fruit": {"name": "cherry", "color": "red"}}}}""".parseJson)))
      }
    }

    "in general" should {
      import _root_.monix.execution.Scheduler.Implicits.global

      import sangria.marshalling.sprayJson._
      import spray.json._

      val QueryType = ObjectType(
        "QueryType",
        fields[Unit, Unit](Field("hello", StringType, resolve = _ => "world")))

      "return extended stream result" in {
        import _root_.monix.reactive.Observable
        import sangria.streaming.monix._

        val SubscriptionType = ObjectType(
          "Subscription",
          fields[Unit, Unit](
            Field.subs(
              "letters",
              OptionType(StringType),
              resolve = _ => Observable("a", "b", "c").map(Action(_)))))

        val schema = Schema(QueryType, subscription = Some(SubscriptionType))

        import sangria.execution.ExecutionScheme.StreamExtended

        val stream = Executor.execute(schema, graphql"subscription { letters }")

        val result = stream.toListL.runToFuture.await(timeout)

        result.map(_.result) should (have(size(3))
          .and(contain("""{"data": {"letters": "a"}}""".parseJson))
          .and(contain("""{"data": {"letters": "b"}}""".parseJson))
          .and(contain("""{"data": {"letters": "c"}}""".parseJson)))
      }

      "validate that all fields are subscription fields" in {
        import _root_.monix.reactive.Observable
        import sangria.streaming.monix._

        val SubscriptionType = ObjectType(
          "Subscription",
          fields[Unit, Unit](
            Field.subs(
              "letters",
              OptionType(StringType),
              resolve = _ => Observable("a").map(Action(_))),
            Field("hello", StringType, resolve = _ => "world"))
        )

        val error = intercept[SchemaValidationException](
          Schema(QueryType, subscription = Some(SubscriptionType)))

        error.violations.map(_.errorMessage) should (have(size(1)).and(contain(
          "Subscription type 'Subscription' may either contain only non-subscription fields or only subscription fields (defined with `Field.subs`). Following fields are non-subscription fields among other subscription fields: 'hello'.")))
      }

      "return first result for default execution scheme" in {
        import _root_.monix.reactive.Observable
        import sangria.streaming.monix._

        val SubscriptionType = ObjectType(
          "Subscription",
          fields[Unit, Unit](
            Field.subs(
              "letters",
              OptionType(StringType),
              resolve = _ => Observable("a", "b").map(Action(_))),
            Field.subs(
              "numbers",
              OptionType(IntType),
              resolve = _ => Observable(1, 2).map(Action(_)))
          )
        )

        val schema = Schema(QueryType, subscription = Some(SubscriptionType))

        val result = Executor
          .execute(
            schema,
            graphql"subscription { letters numbers }",
            queryValidator = QueryValidator.default.withoutValidation[SingleFieldSubscriptions])
          .await(timeout)

        (List(result) should contain).oneOf(
          """{"data":{"letters": "a"}}""".parseJson,
          """{"data":{"letters": "b"}}""".parseJson,
          """{"data":{"numbers": 1}}""".parseJson,
          """{"data":{"numbers": 2}}""".parseJson
        )
      }

      "emit one element for non-stream based subscriptions" in {
        import sangria.streaming.monix._

        val SubscriptionType = ObjectType(
          "Subscription",
          fields[Unit, Unit](
            Field("letters", OptionType(StringType), resolve = _ => Some("a")),
            Field("numbers", IntType, resolve = _ => 10)))

        val schema = Schema(QueryType, subscription = Some(SubscriptionType))

        import sangria.execution.ExecutionScheme.Stream

        val stream = Executor.execute(
          schema,
          graphql"subscription { letters numbers }",
          queryValidator = QueryValidator.default.withoutValidation[SingleFieldSubscriptions])

        val result = stream.toListL.runToFuture.await(timeout)

        result should (have(size(1)).and(
          contain("""{"data": {"letters": "a", "numbers": 10}}""".parseJson)))
      }
    }
  }
}
