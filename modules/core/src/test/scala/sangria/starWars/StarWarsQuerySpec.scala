package sangria.starWars

import sangria.execution.Executor
import sangria.marshalling.InputUnmarshaller
import sangria.schema._
import sangria.macros._
import sangria.starWars.TestSchema.{PrivacyError, StarWarsSchema}
import sangria.starWars.TestData.{CharacterRepo, FriendsResolver}
import sangria.util.FutureResultSupport
import InputUnmarshaller.mapVars
import sangria.validation.QueryValidator

import scala.concurrent.ExecutionContext.Implicits.global
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class StarWarsQuerySpec extends AnyWordSpec with Matchers with FutureResultSupport {
  "Basic Queries" should {
    "Correctly identifies R2-D2 as the hero of the Star Wars Saga" in {
      val query = graphql"""
        query HeroNameQuery {
          hero {
            name
          }
        }
        """

      Executor
        .execute(StarWarsSchema, query, new CharacterRepo, deferredResolver = new FriendsResolver)
        .await should be(Map("data" -> Map("hero" -> Map("name" -> "R2-D2"))))
    }

    "Allows us to query for the ID and friends of R2-D2" in {
      val query = graphql"""
        query HeroNameAndFriendsQuery {
          hero {
            id
            name
            friends {
              name
            }
          }
        }
        """

      Executor
        .execute(StarWarsSchema, query, new CharacterRepo, deferredResolver = new FriendsResolver)
        .await should be(
        Map(
          "data" -> Map(
            "hero" -> Map(
              "id" -> "2001",
              "name" -> "R2-D2",
              "friends" -> List(
                Map("name" -> "Luke Skywalker"),
                Map("name" -> "Han Solo"),
                Map("name" -> "Leia Organa")
              )))))
    }

    "Hero query should succeed even if not all types are referenced indirectly" in {
      val query = graphql"""
        query HeroNameAndFriendsQuery {
          hero {
            id
            name
            friends {
              name
            }
          }
        }
        """

      val HeroOnlyQuery = ObjectType[CharacterRepo, Unit](
        "HeroOnlyQuery",
        fields[CharacterRepo, Unit](
          Field(
            "hero",
            TestSchema.Character,
            arguments = TestSchema.EpisodeArg :: Nil,
            resolve = ctx => ctx.ctx.getHero(ctx.arg(TestSchema.EpisodeArg)))
        )
      )

      val heroOnlySchema =
        Schema(HeroOnlyQuery, additionalTypes = TestSchema.Human :: TestSchema.Droid :: Nil)

      Executor
        .execute(heroOnlySchema, query, new CharacterRepo, deferredResolver = new FriendsResolver)
        .await should be(
        Map(
          "data" -> Map(
            "hero" -> Map(
              "id" -> "2001",
              "name" -> "R2-D2",
              "friends" -> List(
                Map("name" -> "Luke Skywalker"),
                Map("name" -> "Han Solo"),
                Map("name" -> "Leia Organa")
              )))))
    }
  }

  "Nested Queries" should {
    "Allows us to query for the friends of friends of R2-D2" in {
      val query = graphql"""
        query NestedQuery {
          hero {
            name
            friends {
              name
              appearsIn
              friends {
                name
              }
            }
          }
        }
        """

      Executor
        .execute(StarWarsSchema, query, new CharacterRepo, deferredResolver = new FriendsResolver)
        .await should be(
        Map(
          "data" -> Map(
            "hero" -> Map(
              "name" -> "R2-D2",
              "friends" -> List(
                Map(
                  "name" -> "Luke Skywalker",
                  "appearsIn" -> List("NEWHOPE", "EMPIRE", "JEDI"),
                  "friends" -> List(
                    Map("name" -> "Han Solo"),
                    Map("name" -> "Leia Organa"),
                    Map("name" -> "C-3PO"),
                    Map("name" -> "R2-D2")
                  )
                ),
                Map(
                  "name" -> "Han Solo",
                  "appearsIn" -> List("NEWHOPE", "EMPIRE", "JEDI"),
                  "friends" -> List(
                    Map("name" -> "Luke Skywalker"),
                    Map("name" -> "Leia Organa"),
                    Map("name" -> "R2-D2")
                  )
                ),
                Map(
                  "name" -> "Leia Organa",
                  "appearsIn" -> List("NEWHOPE", "EMPIRE", "JEDI"),
                  "friends" -> List(
                    Map("name" -> "Luke Skywalker"),
                    Map("name" -> "Han Solo"),
                    Map("name" -> "C-3PO"),
                    Map("name" -> "R2-D2")
                  )
                )
              )
            )
          )
        )
      )
    }
  }

  "Using IDs and query parameters to refetch objects" should {
    "Allows us to query for Luke Skywalker directly, using his ID" in {
      val query = graphql"""
        query FetchLukeQuery {
          human(id: "1000") {
            name
          }
        }
        """

      Executor
        .execute(StarWarsSchema, query, new CharacterRepo, deferredResolver = new FriendsResolver)
        .await should be(
        Map(
          "data" -> Map(
            "human" -> Map(
              "name" -> "Luke Skywalker"
            )
          )
        ))
    }

    "Allows us to create a generic query, then use it to fetch Luke Skywalker using his ID" in {
      val query = graphql"""
        query FetchSomeIDQuery($$someId: String!) {
          human(id: $$someId) {
            name
          }
        }
        """

      val args = mapVars("someId" -> "1000")

      Executor
        .execute(
          StarWarsSchema,
          query,
          new CharacterRepo,
          variables = args,
          deferredResolver = new FriendsResolver)
        .await should be(
        Map(
          "data" -> Map(
            "human" -> Map(
              "name" -> "Luke Skywalker"
            )
          )
        ))
    }

    "Allows us to create a generic query, then use it to fetch Han Solo using his ID" in {
      val query = graphql"""
        query FetchSomeIDQuery($$someId: String!) {
          human(id: $$someId) {
            name
          }
        }
        """

      val args = mapVars("someId" -> "1002")

      Executor
        .execute(
          StarWarsSchema,
          query,
          new CharacterRepo,
          variables = args,
          deferredResolver = new FriendsResolver)
        .await should be(
        Map(
          "data" -> Map(
            "human" -> Map(
              "name" -> "Han Solo"
            )
          )
        ))
    }

    "Allows us to create a generic query, then pass an invalid ID to get null back" in {
      val query = graphql"""
        query humanQuery($$id: String!) {
          human(id: $$id) {
            name
          }
        }
        """

      val args = mapVars("id" -> "not a valid id")

      Executor
        .execute(
          StarWarsSchema,
          query,
          new CharacterRepo,
          variables = args,
          deferredResolver = new FriendsResolver)
        .await should be(
        Map(
          "data" -> Map(
            "human" -> null
          )
        ))
    }
  }

  "Using aliases to change the key in the response" should {
    "Allows us to query for Luke, changing his key with an alias" in {
      val query =
        graphql"""
          query FetchLukeAliased {
            luke: human(id: "1000") {
              name
            }
          }
        """

      Executor
        .execute(StarWarsSchema, query, new CharacterRepo, deferredResolver = new FriendsResolver)
        .await should be(
        Map(
          "data" -> Map(
            "luke" -> Map("name" -> "Luke Skywalker")
          )
        ))
    }

    "Allows us to query for both Luke and Leia, using two root fields and an alias" in {
      val query =
        graphql"""
          query FetchLukeAndLeiaAliased {
            luke: human(id: "1000") {
              name
            }
            leia: human(id: "1003") {
              name
            }
          }
        """

      Executor
        .execute(StarWarsSchema, query, new CharacterRepo, deferredResolver = new FriendsResolver)
        .await should be(
        Map(
          "data" -> Map(
            "luke" -> Map("name" -> "Luke Skywalker"),
            "leia" -> Map("name" -> "Leia Organa")
          )
        ))
    }
  }

  "Uses fragments to express more complex queries" should {
    "Allows us to query using duplicated content" in {
      val query = graphql"""
        query DuplicateFields {
          luke: human(id: "1000") {
            name
            homePlanet
          }
          leia: human(id: "1003") {
            name
            homePlanet
          }
        }
        """

      Executor
        .execute(StarWarsSchema, query, new CharacterRepo, deferredResolver = new FriendsResolver)
        .await should be(
        Map(
          "data" -> Map(
            "luke" -> Map("name" -> "Luke Skywalker", "homePlanet" -> "Tatooine"),
            "leia" -> Map("name" -> "Leia Organa", "homePlanet" -> "Alderaan")
          )
        ))
    }

    "Allows us to use a fragment to avoid duplicating content" in {
      val query = graphql"""
        query UseFragment {
          luke: human(id: "1000") {
            ...HumanFragment
          }
          leia: human(id: "1003") {
            ...HumanFragment
          }
        }

        fragment HumanFragment on Human {
          name
          homePlanet
        }
        """

      Executor
        .execute(StarWarsSchema, query, new CharacterRepo, deferredResolver = new FriendsResolver)
        .await should be(
        Map(
          "data" -> Map(
            "luke" -> Map("name" -> "Luke Skywalker", "homePlanet" -> "Tatooine"),
            "leia" -> Map("name" -> "Leia Organa", "homePlanet" -> "Alderaan")
          )
        ))
    }
  }

  "Using __typename to find the type of an object" should {
    "Allows us to verify that R2-D2 is a droid" in {
      val query = graphql"""
        query CheckTypeOfR2 {
          hero {
            __typename
            name
          }
        }
        """

      Executor
        .execute(StarWarsSchema, query, new CharacterRepo, deferredResolver = new FriendsResolver)
        .await should be(
        Map(
          "data" -> Map(
            "hero" -> Map("__typename" -> "Droid", "name" -> "R2-D2")
          )
        ))
    }

    "Allows us to verify that Luke is a human" in {
      val query = graphql"""
        query CheckTypeOfLuke {
          hero(episode: EMPIRE) {
            __typename
            name
          }
        }
        """

      Executor
        .execute(StarWarsSchema, query, new CharacterRepo, deferredResolver = new FriendsResolver)
        .await should be(
        Map(
          "data" -> Map(
            "hero" -> Map("__typename" -> "Human", "name" -> "Luke Skywalker")
          )
        ))
    }
  }

  "Reporting errors raised in resolvers" should {
    "Correctly reports error on accessing secretBackstory" in {
      val query = graphql"""
        query HeroNameQuery {
          hero {
            name
            secretBackstory
          }
        }
        """

      val res = Executor
        .execute(StarWarsSchema, query, new CharacterRepo, deferredResolver = new FriendsResolver)
        .await
        .asInstanceOf[Map[String, Any]]

      res("data") should be(Map("hero" -> Map("name" -> "R2-D2", "secretBackstory" -> null)))

      val errors = res("errors").asInstanceOf[Seq[Any]]

      errors should (have(size(1)).and(
        contain(
          Map(
            "message" -> "secretBackstory is secret.",
            "path" -> List("hero", "secretBackstory"),
            "locations" -> Vector(Map("line" -> 5, "column" -> 13))))))
    }

    "Correctly reports error on accessing secretBackstory in a list" in {
      val query = graphql"""
        query HeroNameQuery {
          hero {
            name
            friends {
              name
              secretBackstory
            }
          }
        }
        """

      val res = Executor
        .execute(StarWarsSchema, query, new CharacterRepo, deferredResolver = new FriendsResolver)
        .await
        .asInstanceOf[Map[String, Any]]

      res("data") should be(
        Map(
          "hero" ->
            Map(
              "name" -> "R2-D2",
              "friends" -> Vector(
                Map("name" -> "Luke Skywalker", "secretBackstory" -> null),
                Map("name" -> "Han Solo", "secretBackstory" -> null),
                Map("name" -> "Leia Organa", "secretBackstory" -> null)
              )
            )))

      val errors = res("errors").asInstanceOf[Seq[Any]]

      errors should (have(size(3))
        .and(
          contain(
            Map(
              "message" -> "secretBackstory is secret.",
              "path" -> Vector("hero", "friends", 0, "secretBackstory"),
              "locations" -> Vector(Map("line" -> 7, "column" -> 15)))))
        .and(
          contain(
            Map(
              "message" -> "secretBackstory is secret.",
              "path" -> Vector("hero", "friends", 1, "secretBackstory"),
              "locations" -> Vector(Map("line" -> 7, "column" -> 15)))))
        .and(
          contain(
            Map(
              "message" -> "secretBackstory is secret.",
              "path" -> Vector("hero", "friends", 2, "secretBackstory"),
              "locations" -> Vector(Map("line" -> 7, "column" -> 15))))))
    }

    "Correctly reports error on accessing through an alias" in {
      val query = graphql"""
        query HeroNameQuery {
          mainHero: hero {
            name
            story: secretBackstory
          }
        }
        """

      val res = Executor
        .execute(StarWarsSchema, query, new CharacterRepo, deferredResolver = new FriendsResolver)
        .await
        .asInstanceOf[Map[String, Any]]

      res("data") should be(Map("mainHero" -> Map("name" -> "R2-D2", "story" -> null)))

      val errors = res("errors").asInstanceOf[Seq[Any]]

      errors should (have(size(1)).and(
        contain(
          Map(
            "message" -> "secretBackstory is secret.",
            "path" -> List("mainHero", "story"),
            "locations" -> Vector(Map("line" -> 5, "column" -> 13))))))
    }

    "Full response path is included when fields are non-nullable" in {
      lazy val A: ObjectType[Unit, Any] = ObjectType(
        "A",
        () =>
          fields[Unit, Any](
            Field("nullableA", OptionType(A), resolve = _ => ""),
            Field("nonNullA", A, resolve = _ => ""),
            Field("throws", A, resolve = _ => throw PrivacyError("Catch me if you can"))
          )
      )

      val Query = ObjectType(
        "Query",
        fields[Unit, Unit](Field("nullableA", OptionType(A), resolve = _ => "")))

      val schema = Schema(Query)

      val query = graphql"""
        query {
          nullableA {
            nullableA {
              nonNullA {
                nonNullA {
                  throws
                }
              }
            }
          }
        }
        """

      val res = Executor
        .execute(schema, query, queryValidator = QueryValidator.empty)
        .await
        .asInstanceOf[Map[String, Any]]

      res("data") should be(Map("nullableA" -> Map("nullableA" -> null)))

      val errors = res("errors").asInstanceOf[Seq[Any]]

      errors should (have(size(1)).and(contain(Map(
        "message" -> "Catch me if you can",
        "path" -> List("nullableA", "nullableA", "nonNullA", "nonNullA", "throws"),
        "locations" -> List(Map("line" -> 7, "column" -> 19))
      ))))
    }
  }
}
