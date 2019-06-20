package sangria.execution.deferred

import java.util.concurrent.atomic.AtomicInteger

import org.scalatest.{Matchers, WordSpec}
import sangria.ast
import sangria.execution.Executor
import sangria.macros._
import sangria.schema._
import sangria.util.{FutureResultSupport, Pos}
import sangria.util.SimpleGraphQlSupport._

import scala.concurrent.{ExecutionContext, Future}

class FetcherSpec extends WordSpec with Matchers with FutureResultSupport {
  case class Product(id: Int, name: String, inCategories: Vector[String])
  case class Category(id: String, name: String, children: Seq[String], products: Vector[Int] = Vector.empty)
  case class ColorDeferred(id: String) extends Deferred[String]

  object Category {
    implicit val hasId = HasId[Category, String](_.id)
  }

  object Product {
    implicit val hasId = HasId[Product, Int](_.id)
  }

  val prodCat = Relation[Product, String]("product-category", _.inCategories)
  val prodComplexCat = Relation[Product, (Seq[String], Product),String]("product-category-complex", _._1, _._2)
  val catProd = Relation[Category, Int]("category-product", _.products)

  class Repo {
    private val categories = Vector(
      Category("1", "Root", Vector("2", "3", "4")),
      Category("2", "Cat 2", Vector("5", "6")),
      Category("3", "Cat 3", Vector("7", "5", "6")),
      Category("4", "Cat 4", Vector.empty, Vector(1, 2, 3)),
      Category("5", "Cat 5", Vector.empty, Vector(2, 4)),
      Category("6", "Cat 6", Vector.empty, Vector(5, 6, 1)),
      Category("7", "Cat 7", Vector.empty, Vector(2, 3)),
      Category("8", "Cat 8", Vector("4", "5", "foo!")),
      Category("20", "Cat 8", (1 to 8).map(_.toString)))

    private val products = Vector(
      Product(1, "Rusty sword", Vector("4", "6")),
      Product(2, "Magic belt", Vector("4", "5", "7")),
      Product(3, "Health potion", Vector("4", "7")),
      Product(4, "Unidentified potion", Vector("5")),
      Product(5, "Common boots", Vector("6")),
      Product(6, "Golden ring", Vector("6")))

    def loadCategories(ids: Seq[String])(implicit ec: ExecutionContext): Future[Seq[Category]] =
      Future(ids.flatMap(id ⇒ categories.find(_.id == id)))

    def loadProducts(ids: Seq[Int])(implicit ec: ExecutionContext): Future[Seq[Product]] =
      Future(ids.flatMap(id ⇒ products.find(_.id == id)))

    def loadProductsByCategory(categoryIds: Seq[String])(implicit ec: ExecutionContext): Future[Seq[Product]] =
      Future(products.filter(p ⇒ categoryIds.exists(p.inCategories contains _)))

    def loadCategoriesByProduct(productIds: Seq[Int])(implicit ec: ExecutionContext): Future[Seq[Category]] =
      Future(categories.filter(c ⇒ productIds.exists(c.products contains _)))

    def getCategory(id: String)(implicit ec: ExecutionContext) =
      Future(categories.find(_.id == id))

    def getProduct(id: Int)(implicit ec: ExecutionContext) =
      Future(products.find(_.id == id))
  }

  def properFetcher(implicit ec: ExecutionContext) = {
    val defaultCatFetcher = Fetcher.relCaching[Repo, Category, Category, String](
      (repo, ids) ⇒ repo.loadCategories(ids),
      (repo, ids) ⇒ repo.loadCategoriesByProduct(ids(catProd)))

    val defaultProdFetcher = Fetcher.relCaching[Repo, Product, Product, Int](
      (repo, ids) ⇒ repo.loadProducts(ids),
      (repo, ids) ⇒ repo.loadProductsByCategory(ids(prodCat)))

    val complexProdFetcher = Fetcher.relCaching[Repo, Product, (Seq[String], Product), Int](
      (repo, ids) ⇒ repo.loadProducts(ids),
      (repo, ids) ⇒ repo.loadProductsByCategory(ids(prodComplexCat)).map(_.map(p ⇒ p.inCategories → p)))

    val defaultResolver = DeferredResolver.fetchers(defaultProdFetcher, defaultCatFetcher)

    def schema(fetcherCat: Fetcher[Repo, Category, Category, String] = defaultCatFetcher, fetcherProd: Fetcher[Repo, Product, Product, Int] = defaultProdFetcher) = {
      lazy val ProductType: ObjectType[Repo, Product] = ObjectType("Product", () ⇒ fields(
        Field("id", IntType, resolve = c ⇒ c.value.id),
        Field("name", StringType, resolve = c ⇒ c.value.name),
        Field("categories", ListType(CategoryType),
          resolve = c ⇒ fetcherCat.deferSeqOpt(c.value.inCategories)),
        Field("categoryRel", CategoryType,
          resolve = c ⇒ fetcherCat.deferRel(catProd, c.value.id)),
        Field("categoryRelOpt", OptionType(CategoryType),
          resolve = c ⇒ fetcherCat.deferRelOpt(catProd, c.value.id)),
        Field("categoryRelSeq", ListType(CategoryType),
          resolve = c ⇒ fetcherCat.deferRelSeq(catProd, c.value.id))))

      lazy val CategoryType: ObjectType[Repo, Category] = ObjectType("Category", () ⇒ fields(
        Field("id", StringType, resolve = c ⇒ c.value.id),
        Field("name", StringType, resolve = c ⇒ c.value.name),
        Field("color", StringType, resolve = c ⇒ ColorDeferred("red")),
        Field("self", CategoryType, resolve = c ⇒ c.value),
        Field("selfOpt", OptionType(CategoryType), resolve = c ⇒ Some(c.value)),
        Field("selfFut", CategoryType, resolve = c ⇒ Future(c.value)),
        Field("products", ListType(ProductType),
          resolve = c ⇒ fetcherProd.deferSeqOpt(c.value.products)),
        Field("productRel", ProductType,
          resolve = c ⇒ fetcherProd.deferRel(prodCat, c.value.id)),
        Field("productComplexRel", ListType(ProductType),
          resolve = c ⇒ complexProdFetcher.deferRelSeq(prodComplexCat, c.value.id)),
        Field("productRelOpt", OptionType(ProductType),
          resolve = c ⇒ fetcherProd.deferRelOpt(prodCat, c.value.id)),
        Field("productRelSeq", ListType(ProductType),
          resolve = c ⇒ fetcherProd.deferRelSeq(prodCat, c.value.id)),
        Field("categoryNonOpt", CategoryType,
          arguments = Argument("id", StringType) :: Nil,
          resolve = c ⇒ fetcherCat.defer(c.arg[String]("id"))),
        Field("childrenSeq", ListType(CategoryType),
          resolve = c ⇒ fetcherCat.deferSeq(c.value.children)),
        Field("childrenSeqOpt", ListType(CategoryType),
          resolve = c ⇒ fetcherCat.deferSeqOpt(c.value.children)),
        Field("childrenFut", ListType(CategoryType),
          resolve = c ⇒ Future.successful(
            fetcherCat.deferSeq(c.value.children)))))

      val QueryType = ObjectType("Query", fields[Repo, Unit](
        Field("category", OptionType(CategoryType),
          arguments = Argument("id", StringType) :: Nil,
          resolve = c ⇒ fetcherCat.deferOpt(c.arg[String]("id"))),
        Field("categoryEager", OptionType(CategoryType),
          arguments = Argument("id", StringType) :: Nil,
          resolve = c ⇒ c.ctx.getCategory(c.arg[String]("id"))),
        Field("categoryAndCacheEmptyProducts", CategoryType,
          arguments = Argument("id", StringType) :: Nil,
          resolve = c ⇒ DeferredValue(fetcherCat.defer(c.arg[String]("id"))) map {
            cat => {
              fetcherProd.updateCacheRel(c.deferredResolverState, prodCat, cat.id, Seq())
              cat
            }
          }),
        Field("categoryNonOpt", CategoryType,
          arguments = Argument("id", StringType) :: Nil,
          resolve = c ⇒ fetcherCat.defer(c.arg[String]("id"))),
        Field("products", ListType(ProductType),
          arguments = Argument("categoryIds", ListInputType(StringType)) :: Nil,
          resolve = c ⇒ fetcherProd.deferRelSeqMany(prodCat, c.arg[Seq[String]]("categoryIds"))),
        Field("productOpt", OptionType(ProductType),
          arguments = Argument("id", OptionInputType(IntType)) :: Nil,
          resolve = c ⇒ fetcherProd.deferOpt(c.argOpt[Int]("id"))),
        Field("productsOptExplicit", ListType(OptionType(ProductType)),
          arguments = Argument("ids", ListInputType(IntType)) :: Nil,
          resolve = c ⇒ fetcherProd.deferSeqOptExplicit(c.arg[Seq[Int]]("ids"))),
        Field("root", CategoryType, resolve = _ ⇒ fetcherCat.defer("1")),
        Field("rootFut", CategoryType, resolve = _ ⇒
          Future.successful(fetcherCat.defer("1")))))

      Schema(QueryType)
    }

    "fetch results in batches and cache results if necessary" in {
      val query =
        graphql"""
          {
            c1: category(id: "non-existing") {name}
            c3: category(id: "8") {name childrenSeqOpt {id}}

            rootFut {
              id
              name
              childrenSeq {
                id
                name
                childrenSeq {
                  id
                  name
                  childrenSeq {
                    id
                    name
                    childrenSeq {
                      id
                      name
                    }
                  }
                }
              }
            }
          }
        """

      var fetchedIds = Vector.empty[Seq[String]]

      val fetcher =
        Fetcher((repo: Repo, ids: Seq[String]) ⇒ {
          fetchedIds = fetchedIds :+ ids

          repo.loadCategories(ids)
        })

      var fetchedIdsCached = Vector.empty[Seq[String]]

      val fetcherCached =
        Fetcher.caching((repo: Repo, ids: Seq[String]) ⇒ {
          fetchedIdsCached = fetchedIdsCached :+ ids

          repo.loadCategories(ids)
        })


      val res = Executor.execute(schema(fetcher), query, new Repo,
        deferredResolver = DeferredResolver.fetchers(fetcher)).await

      val resCached = Executor.execute(schema(fetcherCached), query, new Repo,
        deferredResolver = DeferredResolver.fetchers(fetcherCached)).await

      fetchedIds should be (Vector(
        Vector("1", "non-existing", "8"),
        Vector("3", "4", "5", "2", "foo!"),
        Vector("5", "6", "7")))

      fetchedIdsCached should be (Vector(
        Vector("1", "non-existing", "8"),
        Vector("3", "4", "5", "2", "foo!"),
        Vector("6", "7")))

      List(res, resCached) foreach (_ should be (
        Map(
          "data" → Map(
            "c1" → null,
            "c3" → Map(
              "name" → "Cat 8",
              "childrenSeqOpt" → Vector(
                Map(
                  "id" → "4"),
                Map(
                  "id" → "5"))),
            "rootFut" → Map(
              "id" → "1",
              "name" → "Root",
              "childrenSeq" → Vector(
                Map(
                  "id" → "2",
                  "name" → "Cat 2",
                  "childrenSeq" → Vector(
                    Map(
                      "id" → "5",
                      "name" → "Cat 5",
                      "childrenSeq" → Vector.empty),
                    Map(
                      "id" → "6",
                      "name" → "Cat 6",
                      "childrenSeq" → Vector.empty))),
                Map(
                  "id" → "3",
                  "name" → "Cat 3",
                  "childrenSeq" → Vector(
                    Map(
                      "id" → "7",
                      "name" → "Cat 7",
                      "childrenSeq" → Vector.empty),
                    Map(
                      "id" → "5",
                      "name" → "Cat 5",
                      "childrenSeq" → Vector.empty),
                    Map(
                      "id" → "6",
                      "name" → "Cat 6",
                      "childrenSeq" → Vector.empty))),
                Map(
                  "id" → "4",
                  "name" → "Cat 4",
                  "childrenSeq" → Vector.empty)))))))
    }

    "fetch results with `deferOpt` and option argument" in {
      val query =
        graphql"""
          {
            p1: productOpt(id: 1) {id, name}
            p2: productOpt {id, name}
            p3: productOpt(id: 12345) {id, name}
          }
        """

      val res = Executor.execute(schema(), query, new Repo, deferredResolver = defaultResolver).await

      res should be (
        Map(
          "data" → Map(
            "p1" → Map(
              "id" → 1,
              "name" → "Rusty sword"),
            "p2" → null,
            "p3" → null)))
    }

    "fetch results with `deferSeqOptExplicit`" in {
      val query =
        graphql"""
          {
            productsOptExplicit(ids: [1, 1001, 2, 3, 3001]) {id, name}
          }
        """

      Executor.execute(schema(), query, new Repo, deferredResolver = defaultResolver).await should be (
        Map(
          "data" → Map(
            "productsOptExplicit" → Vector(
              Map("id" → 1, "name" → "Rusty sword"),
              null,
              Map("id" → 2, "name" → "Magic belt"),
              Map("id" → 3, "name" → "Health potion"),
              null))))
    }

    "cache relation results" in {
      val query =
        graphql"""
          {
            p1: products(categoryIds: ["4", "7"]) {
              categoryRel {
                name
                products {
                  categoryRel {
                    name
                    products {
                      categoryRel {
                        name
                      }
                    }
                  }
                }
              }
            }
          }
        """

      var fetchedIds = Vector.empty[Seq[String]]
      var fetchedRels = Vector.empty[RelationIds[Category]]

      val fetcher =
        Fetcher.rel(
          (repo: Repo, ids: Seq[String]) ⇒ {
            fetchedIds = fetchedIds :+ ids

            repo.loadCategories(ids)
          },
          (repo: Repo, ids: RelationIds[Category]) ⇒ {
            fetchedRels = fetchedRels :+ ids

            repo.loadCategoriesByProduct(ids(catProd))
          })

      var fetchedIdsCached = Vector.empty[Seq[String]]
      var fetchedRelsCached = Vector.empty[RelationIds[Category]]

      val fetcherCached =
        Fetcher.relCaching(
          (repo: Repo, ids: Seq[String]) ⇒ {
            fetchedIdsCached = fetchedIdsCached :+ ids

            repo.loadCategories(ids)
          },
          (repo: Repo, ids: RelationIds[Category]) ⇒ {
            fetchedRelsCached = fetchedRelsCached :+ ids

            repo.loadCategoriesByProduct(ids(catProd))
          })

      var fetchedRelsOnly = Vector.empty[RelationIds[Category]]

      val fetcherRelsOnly =
        Fetcher.relOnly(
          (repo: Repo, ids: RelationIds[Category]) ⇒ {
            fetchedRelsOnly = fetchedRelsOnly :+ ids

            repo.loadCategoriesByProduct(ids(catProd))
          })

      var fetchedRelsOnlyCached = Vector.empty[RelationIds[Category]]

      val fetcherRelsOnlyCached =
        Fetcher.relOnlyCaching(
          (repo: Repo, ids: RelationIds[Category]) ⇒ {
            fetchedRelsOnlyCached = fetchedRelsOnlyCached :+ ids

            repo.loadCategoriesByProduct(ids(catProd))
          })

      val res = Executor.execute(schema(fetcher), query, new Repo,
        deferredResolver = DeferredResolver.fetchers(fetcher, defaultProdFetcher)).await

      val resCached = Executor.execute(schema(fetcherCached), query, new Repo,
        deferredResolver = DeferredResolver.fetchers(fetcherCached, defaultProdFetcher)).await

      val resRelsOnly = Executor.execute(schema(fetcherRelsOnly), query, new Repo,
        deferredResolver = DeferredResolver.fetchers(fetcherRelsOnly, defaultProdFetcher)).await

      val resRelsOnlyCached = Executor.execute(schema(fetcherRelsOnlyCached), query, new Repo,
        deferredResolver = DeferredResolver.fetchers(fetcherRelsOnlyCached, defaultProdFetcher)).await

      fetchedIds should have size 0
      fetchedIdsCached should have size 0

      val relsOut = Vector(
        RelationIds[Category](Map(catProd → Vector(1, 2, 3))),
        RelationIds[Category](Map(catProd → Vector(1, 2, 3))),
        RelationIds[Category](Map(catProd → Vector(1, 2, 3))))

      val relsCachedOut = Vector(RelationIds[Category](Map(catProd → Vector(1, 2, 3))))

      fetchedRels should be (relsOut)

      fetchedRelsCached should be (relsCachedOut)

      fetchedRelsOnly should be (relsOut)

      fetchedRelsOnlyCached should be (relsCachedOut)

      res should be (resCached)

      resRelsOnly should be (resRelsOnlyCached)
    }

    "handle complex relations" in {
      val query =
        graphql"""
          {
            c1: category(id: "5") {
              productComplexRel {
                id
              }
            }

            c2: category(id: "6") {
              productComplexRel {
                name
              }
            }
          }
        """

      val res = Executor.execute(schema(), query, new Repo,
        deferredResolver = DeferredResolver.fetchers(complexProdFetcher, defaultProdFetcher, defaultCatFetcher)).await

      res should be (Map(
        "data" → Map(
          "c1" → Map(
            "productComplexRel" → Vector(
              Map(
                "id" → 2),
              Map(
                "id" → 4))),
          "c2" → Map(
            "productComplexRel" → Vector(
              Map(
                "name" → "Rusty sword"),
              Map(
                "name" → "Common boots"),
              Map(
                "name" → "Golden ring"))))))
    }

    "should result in error for missing non-optional values" in {
      var fetchedIds = Vector.empty[Seq[String]]

      val fetcher =
        Fetcher((repo: Repo, ids: Seq[String]) ⇒ {
          fetchedIds = fetchedIds :+ ids

          repo.loadCategories(ids)
        })

      checkContainsErrors(schema(fetcher), (),
        """
          {
            c1: category(id: "8") {name childrenSeq {id}}
            c2: categoryEager(id: "1") {
              name
              selfOpt {
                categoryNonOpt(id: "qwe") {name}
              }
            }
          }
        """,
        Map(
          "c1" → null,
          "c2" → Map(
            "name" → "Root",
            "selfOpt" → null)),
        List(
          "Fetcher has not resolved non-optional ID 'foo!'." → List(Pos(3, 41)),
          "Fetcher has not resolved non-optional ID 'qwe'." → List(Pos(7, 17))),
        resolver = DeferredResolver.fetchers(fetcher),
        userContext = new Repo)

      fetchedIds should be (Vector(
        Vector("8", "qwe"),
        Vector("4", "5", "foo!")))
    }

    "use fallback `DeferredResolver`" in {
      class MyDeferredResolver extends DeferredResolver[Any] {
        val callsCount = new AtomicInteger(0)
        val valueCount = new AtomicInteger(0)

        override val includeDeferredFromField: Option[(Field[_, _], Vector[ast.Field], Args, Double) ⇒ Boolean] =
          Some((_, _, _, _) ⇒ false)

        def resolve(deferred: Vector[Deferred[Any]], ctx: Any, queryState: Any)(implicit ec: ExecutionContext) = {
          callsCount.getAndIncrement()
          valueCount.addAndGet(deferred.size)

          deferred.map {
            case ColorDeferred(id) ⇒ Future.successful(id + "Color")
          }
        }
      }

      check(schema(), (),
        """
          {
            c1: category(id: "1") {name childrenSeq {id}}


            c2: categoryEager(id: "2") {
              color
              childrenSeq {name}
            }
          }
        """,
        Map(
          "data" → Map(
            "c1" → Map(
              "name" → "Root",
              "childrenSeq" → Vector(
                Map("id" → "2"),
                Map("id" → "3"),
                Map("id" → "4"))),
            "c2" → Map(
              "color" → "redColor",
              "childrenSeq" → Vector(
                Map("name" → "Cat 5"),
                Map("name" → "Cat 6"))))),
        resolver = DeferredResolver.fetchersWithFallback(new MyDeferredResolver, defaultCatFetcher, defaultProdFetcher),
        userContext = new Repo)
    }

    "explicit cache should be used in consequent executions" in {
      var fetchedIds = Vector.empty[Seq[String]]
      val cache = FetcherCache.simple

      (1 to 3) foreach { _ ⇒
        val fetcher = Fetcher.caching(
          config = FetcherConfig.caching(cache),
          fetch = (repo: Repo, ids: Seq[String]) ⇒ {
            fetchedIds = fetchedIds :+ ids

            repo.loadCategories(ids)
          })

        check(schema(fetcher), (),
          """
            {
              root {
                childrenSeq {
                  childrenSeq {
                    childrenSeq {
                      childrenSeq {
                        name
                      }
                    }
                  }
                }
              }
            }
          """,
          Map(
            "data" → Map(
              "root" → Map(
                "childrenSeq" → Vector(
                  Map(
                    "childrenSeq" → Vector(
                      Map(
                        "childrenSeq" → Vector.empty),
                      Map(
                        "childrenSeq" → Vector.empty))),
                  Map(
                    "childrenSeq" → Vector(
                      Map(
                        "childrenSeq" → Vector.empty),
                      Map(
                        "childrenSeq" → Vector.empty),
                      Map(
                        "childrenSeq" → Vector.empty))),
                  Map(
                    "childrenSeq" → Vector.empty))))),
          resolver = DeferredResolver.fetchers(fetcher),
          userContext = new Repo)
      }

      fetchedIds should be (Vector(
        Vector("1"),
        Vector("3", "4", "2"),
        Vector("5", "6", "7")))
    }

    "support multiple fetchers" in {
      var fetchedCatIds = Vector.empty[Seq[String]]

      val fetcherCat =
        Fetcher((repo: Repo, ids: Seq[String]) ⇒ {
          fetchedCatIds = fetchedCatIds :+ ids

          repo.loadCategories(ids)
        })

      var fetchedProdIds = Vector.empty[Seq[Int]]

      val fetcherProd =
        Fetcher((repo: Repo, ids: Seq[Int]) ⇒ {
          fetchedProdIds = fetchedProdIds :+ ids

          repo.loadProducts(ids)
        })

      check(schema(fetcherCat, fetcherProd), (),
        """
          {
            root {
              ...Cat
              childrenSeq {
                ...Cat
                childrenSeq {
                  ...Cat
                }
              }
            }
          }

          fragment Cat on Category {
            name
            products {
              name
              categories {
                name
              }
            }
          }
        """,
        Map(
          "data" → Map(
            "root" → Map(
              "name" → "Root",
              "products" → Vector.empty,
              "childrenSeq" → Vector(
                Map(
                  "name" → "Cat 2",
                  "products" → Vector.empty,
                  "childrenSeq" → Vector(
                    Map(
                      "name" → "Cat 5",
                      "products" → Vector(
                        Map(
                          "name" → "Magic belt",
                          "categories" → Vector(
                            Map("name" → "Cat 4"),
                            Map("name" → "Cat 5"),
                            Map("name" → "Cat 7"))),
                        Map(
                          "name" → "Unidentified potion",
                          "categories" → Vector(
                            Map("name" → "Cat 5"))))),
                    Map(
                      "name" → "Cat 6",
                      "products" → Vector(
                        Map(
                          "name" → "Common boots",
                          "categories" → Vector(
                            Map("name" → "Cat 6"))),
                        Map(
                          "name" → "Golden ring",
                          "categories" → Vector(
                            Map("name" → "Cat 6"))),
                        Map(
                          "name" → "Rusty sword",
                          "categories" → Vector(
                            Map("name" → "Cat 4"),
                            Map("name" → "Cat 6"))))))),
                Map(
                  "name" → "Cat 3",
                  "products" → Vector.empty,
                  "childrenSeq" → Vector(
                    Map(
                      "name" → "Cat 7",
                      "products" → Vector(
                        Map(
                          "name" → "Magic belt",
                          "categories" → Vector(
                            Map("name" → "Cat 4"),
                            Map("name" → "Cat 5"),
                            Map("name" → "Cat 7"))),
                        Map(
                          "name" → "Health potion",
                          "categories" → Vector(
                            Map("name" → "Cat 4"),
                            Map("name" → "Cat 7"))))),
                    Map(
                      "name" → "Cat 5",
                      "products" → Vector(
                        Map(
                          "name" → "Magic belt",
                          "categories" → Vector(
                            Map("name" → "Cat 4"),
                            Map("name" → "Cat 5"),
                            Map("name" → "Cat 7"))),
                        Map(
                          "name" → "Unidentified potion",
                          "categories" → Vector(
                            Map("name" → "Cat 5"))))),
                    Map(
                      "name" → "Cat 6",
                      "products" → Vector(
                        Map(
                          "name" → "Common boots",
                          "categories" → Vector(
                            Map("name" → "Cat 6"))),
                        Map(
                          "name" → "Golden ring",
                          "categories" → Vector(
                            Map("name" → "Cat 6"))),
                        Map(
                          "name" → "Rusty sword",
                          "categories" → Vector(
                            Map("name" → "Cat 4"),
                            Map("name" → "Cat 6"))))))),
                Map(
                  "name" → "Cat 4",
                  "products" → Vector(
                    Map(
                      "name" → "Rusty sword",
                      "categories" → Vector(
                        Map("name" → "Cat 4"),
                        Map("name" → "Cat 6"))),
                    Map(
                      "name" → "Magic belt",
                      "categories" → Vector(
                        Map("name" → "Cat 4"),
                        Map("name" → "Cat 5"),
                        Map("name" → "Cat 7"))),
                    Map(
                      "name" → "Health potion",
                      "categories" → Vector(
                        Map("name" → "Cat 4"),
                        Map("name" → "Cat 7")))),
                  "childrenSeq" → Vector.empty))))),
        resolver = DeferredResolver.fetchers(fetcherCat, fetcherProd),
        userContext = new Repo)

      fetchedCatIds should be (Vector(
        Vector("1"),
        Vector("3", "4", "2"),
        Vector("5", "6", "7"),
        Vector("4", "5", "6", "7"),
        Vector("4", "5", "6", "7")))

      fetchedProdIds.map(_.sorted) should be (Vector(
        Vector(1, 2, 3),
        Vector(1, 2, 3, 4, 5, 6)))
    }

    "support a single relation" in check(schema(), (),
      """
        {
          category(id: "4") {
            productRel {
              name

              categoryRel {
                name
              }
            }
          }
        }
      """,
      Map(
        "data" → Map(
          "category" → Map(
            "productRel" → Map(
              "name" → "Rusty sword",
              "categoryRel" → Map(
                "name" → "Cat 4"))))),
      resolver = defaultResolver,
      userContext = new Repo)

    "single relation should produce an error if value is not resolved" in checkContainsErrors(schema(), (),
      """
        {
          category(id: "1") {
            productRel {
              name
            }
          }
        }
      """,
      Map("category" → null),
      List("Fetcher has not resolved non-optional relation ID '1' for relation 'SimpleRelation(product-category)'." → List(Pos(4, 13))),
      resolver = defaultResolver,
      userContext = new Repo)

    "support a optional and list relations" in check(schema(), (),
      """
        {
          c1: category(id: "1") {
            productRelOpt {
              name
            }

            productRelSeq {
              name
            }
          }

          c2: category(id: "4") {
            productRelOpt {
              name

              categoryRelOpt {
                name
              }
            }

            productRelSeq {
              name

              categoryRelSeq {
                name
              }
            }
          }
        }
      """,
      Map(
        "data" → Map(
          "c1" → Map(
            "productRelOpt" → null,
            "productRelSeq" → Vector.empty),
          "c2" → Map(
            "productRelOpt" → Map(
              "name" → "Rusty sword",
              "categoryRelOpt" → Map(
                "name" → "Cat 4")),
            "productRelSeq" → Vector(
              Map(
                "name" → "Rusty sword",
                "categoryRelSeq" → Vector(
                  Map("name" → "Cat 4"),
                  Map("name" → "Cat 6"))),
              Map(
                "name" → "Magic belt",
                "categoryRelSeq" → Vector(
                  Map("name" → "Cat 4"),
                  Map("name" → "Cat 5"),
                  Map("name" → "Cat 7"))),
              Map(
                "name" → "Health potion",
                "categoryRelSeq" → Vector(
                  Map("name" → "Cat 4"),
                  Map("name" → "Cat 7"))))))),
      resolver = defaultResolver,
      userContext = new Repo)

    "support multiple relations" in check(schema(), (),
      """
        {
          products(categoryIds: ["1", "2", "5", "6", "4"]) {
            id, name
          }
        }
      """,
      Map(
        "data" → Map(
          "products" → Vector(
            Map(
              "id" → 2,
              "name" → "Magic belt"),
            Map(
              "id" → 4,
              "name" → "Unidentified potion"),
            Map(
              "id" → 1,
              "name" → "Rusty sword"),
            Map(
              "id" → 5,
              "name" → "Common boots"),
            Map(
              "id" → 6,
              "name" → "Golden ring"),
            Map(
              "id" → 3,
              "name" → "Health potion")))),
      resolver = defaultResolver,
      userContext = new Repo)

    "support manual cache updates" in {
      var fetchedProdIds = Vector.empty[Seq[Int]]

      val fetcherProd =
        Fetcher.cachingWithContext[Repo, Product, Int] { (c, ids) ⇒
          fetchedProdIds = fetchedProdIds :+ ids

          c.ctx.loadProducts(ids)
        }

      val fetcherCat =
        Fetcher.cachingWithContext[Repo, Category, String] { (c, ids) ⇒
          c.ctx.loadCategories(ids).map { categories ⇒
            c.cacheFor(fetcherProd).foreach { productCache ⇒
              productCache.update(4, Product(4, "Manually Cached", categories.map(_.id).toVector))
            }
            
            categories
          }
        }
      
      check(schema(fetcherCat, fetcherProd), (),
        """
          {
            category(id: "5") {
              name

              products {
                name
              }
            }
          }
        """,
        Map(
          "data" → Map(
            "category" → Map(
              "name" → "Cat 5",
              "products" → Vector(
                Map("name" → "Magic belt"),
                Map("name" → "Manually Cached"))))),
        resolver = DeferredResolver.fetchers(fetcherCat, fetcherProd),
        userContext = new Repo)

      fetchedProdIds.map(_.sorted) should be (Vector(Vector(2)))
    }

    "support manual cache updates at field resolver" in {
      check(schema(), (), """
          {
            categoryAndCacheEmptyProducts(id: "4") {
              name
              productRelSeq {
                name
              }
            }
          }
        """, Map(
        "data" → Map(
          "categoryAndCacheEmptyProducts" → Map(
            "name" → "Cat 4",
            "productRelSeq" → Vector(),
          ))),
        resolver = defaultResolver,
        userContext = new Repo)
    }
  }

  "Fetcher" when {
    "using standard execution context" should {
      behave like properFetcher (ExecutionContext.Implicits.global)
    }

    "using sync execution context" should {
      behave like properFetcher (sync.executionContext)
    }
  }
}
