package sangria.execution.deferred

import java.util.concurrent.atomic.AtomicInteger
import sangria.ast
import sangria.execution.{DeferredWithInfo, Executor}
import sangria.macros._
import sangria.schema._
import sangria.util.{FutureResultSupport, Pos}
import sangria.util.SimpleGraphQlSupport._

import scala.concurrent.{ExecutionContext, Future}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DeferredResolverSpec extends AnyWordSpec with Matchers with FutureResultSupport {
  private[this] def deferredResolver(implicit ec: ExecutionContext): Unit = {
    case class LoadCategories(ids: Seq[String]) extends Deferred[Seq[String]]

    lazy val CategoryType: ObjectType[Unit, String] = ObjectType(
      "Category",
      () =>
        fields[Unit, String](
          Field("name", StringType, resolve = c => s"Cat ${c.value}"),
          Field("descr", StringType, resolve = c => s"Cat ${c.value} descr"),
          Field("self", CategoryType, resolve = c => c.value),
          Field("selfFut", CategoryType, resolve = c => Future(c.value)),
          Field(
            "selfFutComplex",
            CategoryType,
            complexity = Some((_, _, _) => 1000),
            resolve = c => Future(c.value)),
          Field(
            "children",
            ListType(CategoryType),
            arguments = Argument("count", IntType) :: Nil,
            resolve = c => LoadCategories((1 to c.arg[Int]("count")).map(i => s"${c.value}.$i"))
          ),
          Field(
            "childrenComplex",
            ListType(CategoryType),
            complexity = Some((_, _, _) => 1000),
            arguments = Argument("count", IntType) :: Nil,
            resolve = c => LoadCategories((1 to c.arg[Int]("count")).map(i => s"${c.value}.$i"))
          ),
          Field(
            "childrenFut",
            ListType(CategoryType),
            arguments = Argument("count", IntType) :: Nil,
            resolve = c =>
              DeferredFutureValue(
                Future.successful(LoadCategories((1 to c.arg[Int]("count")).map(i =>
                  s"${c.value}.$i"))))
          )
        )
    )

    val QueryType = ObjectType(
      "Query",
      fields[Unit, Unit](
        Field(
          "root",
          CategoryType,
          resolve = _ => DeferredValue(LoadCategories(Seq("root"))).map(_.head)),
        Field(
          "rootFut",
          CategoryType,
          resolve =
            _ => DeferredFutureValue(Future.successful(LoadCategories(Seq("root")))).map(_.head)),
        Field(
          "fail1",
          OptionType(CategoryType),
          resolve = _ => DeferredValue(LoadCategories(Seq("fail"))).map(_.head)),
        Field(
          "fail2",
          OptionType(CategoryType),
          resolve = _ => DeferredValue(LoadCategories(Seq("fail"))).map(_.head))
      )
    )

    val MutationType = ObjectType(
      "Mutation",
      fields[Unit, Unit](
        Field(
          "root",
          OptionType(CategoryType),
          resolve = _ => DeferredValue(LoadCategories(Seq("root"))).map(_.head)),
        Field(
          "fail1",
          OptionType(CategoryType),
          resolve = _ => DeferredValue(LoadCategories(Seq("fail"))).map(_.head)),
        Field(
          "fail2",
          OptionType(CategoryType),
          resolve = _ => DeferredValue(LoadCategories(Seq("fail"))).map(_.head))
      )
    )

    class MyDeferredResolver extends DeferredResolver[Any] {
      val callsCount = new AtomicInteger(0)
      val valueCount = new AtomicInteger(0)

      override val includeDeferredFromField
          : Option[(Field[_, _], Vector[ast.Field], Args, Double) => Boolean] =
        Some((_, _, _, complexity) => complexity < 100)

      override def groupDeferred[T <: DeferredWithInfo](deferred: Vector[T]): Vector[Vector[T]] = {
        val (expensive, cheap) = deferred.partition(_.complexity > 100)
        Vector(expensive, cheap)
      }

      override def resolve(deferred: Vector[Deferred[Any]], ctx: Any, queryState: Any)(implicit
          ec: ExecutionContext): Vector[Future[Seq[String]]] = {
        callsCount.getAndIncrement()
        valueCount.addAndGet(deferred.size)

        deferred.map {
          case LoadCategories(ids) if ids contains "fail" =>
            Future.failed(new IllegalStateException("foo"))
          case LoadCategories(ids) => Future.successful(ids)
        }
      }
    }

    val schema = Schema(QueryType, Some(MutationType))

    def exec(query: ast.Document) = {
      val resolver = new MyDeferredResolver
      val result = Executor.execute(schema, query, deferredResolver = resolver).await

      resolver -> result
    }

    "result in a single resolution of once level" in {
      val query =
        graphql"""
          {
            root {
              name
              children(count: 5) {
                children(count: 5) {
                  children(count: 5) {
                    children(count: 5) {
                      children(count: 5) {
                        name
                      }
                    }

                    childrenFut(count: 2) {
                      children(count: 2) {
                        name
                      }
                    }

                    self {
                      children(count: 3) {
                        children(count: 3) {
                          name
                        }
                      }
                    }

                    selfFut {
                      children(count: 3) {
                        children(count: 3) {
                          name
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        """

      val (resolver, _) = exec(query)

      resolver.callsCount.get should be(6)
      resolver.valueCount.get should be(2157)
    }

    "do not wait for future values" in {
      val query =
        graphql"""
          {
            root {
              name

              children(count: 3) {
                s1: selfFutComplex {
                  children(count: 5) {
                    children(count: 5) {
                      name
                    }
                  }
                }

                s2: selfFutComplex {
                  children(count: 5) {
                    children(count: 5) {
                      name
                    }
                  }
                }

                selfFut {
                  children(count: 5) {
                    children(count: 5) {
                      name
                    }
                  }
                }

                selfFut {
                  children(count: 5) {
                    children(count: 5) {
                      name
                    }
                  }
                }
              }
            }
          }
        """

      val (resolver, _) = exec(query)

      resolver.callsCount.get should be(16)
      resolver.valueCount.get should be(56)
    }

    "Group complex/expensive deferred values together" in {
      val query =
        graphql"""
          {
            rootFut {
              name

              c1: childrenComplex(count: 5) {
                self {
                  childrenFut(count: 5) {
                    name
                  }
                }
              }

              c2: childrenComplex(count: 5) {
                self {
                  childrenFut(count: 5) {
                    name
                  }
                }
              }

              childrenFut(count: 5) {
                self {
                  childrenFut(count: 5) {
                    name
                  }
                }
              }
            }
          }
        """

      val (resolver, r) = exec(query)

      resolver.callsCount.get should be(5)
      resolver.valueCount.get should be(19)
    }

    "failed queries should be handled appropriately" in checkContainsErrors(
      schema,
      (),
      """
        {
          fail1 {name}
          root {name}
          fail2 {name}
        }
      """,
      Some(Map("fail1" -> null, "root" -> Map("name" -> "Cat root"), "fail2" -> null)),
      List("foo" -> List(Pos(3, 11)), "foo" -> List(Pos(5, 11))),
      resolver = new MyDeferredResolver
    )

    "failed mutations should be handled appropriately" in checkContainsErrors(
      schema,
      (),
      """
        mutation {
          fail1 {name}
          root {name}
          fail2 {name}
        }
      """,
      Some(Map("fail1" -> null, "root" -> Map("name" -> "Cat root"), "fail2" -> null)),
      List("foo" -> List(Pos(3, 11)), "foo" -> List(Pos(5, 11))),
      resolver = new MyDeferredResolver
    )
  }

  "DeferredResolver" when {
    "using standard execution context" should
      behave.like(deferredResolver(ExecutionContext.Implicits.global))

    "using sync execution context" should
      behave.like(deferredResolver(sync.executionContext))
  }
}
