package sangria.validation.rules

import sangria.util.{Pos, ValidationSupport}
import org.scalatest.wordspec.AnyWordSpec
import sangria.validation.rules.KnownTypeNames.SuggestionFunction

class KnownTypeNamesDisabledSpec extends AnyWordSpec with ValidationSupport {

  override val defaultRule = Some(new KnownTypeNames(suggestion = SuggestionFunction.Disabled))

  "Validate: Known type names" should {
    "known type names are valid" in expectPasses(
      """
        query Foo($var: String, $required: [String!]!) {
          user(id: 4) {
            pets { ... on Pet { name }, ...PetFields, ... { name }}
          }
        }
        fragment PetFields on Pet {
          name
        }
      """)

    "unknown type names are invalid" in expectFails(
      """
        query Foo($var: JumbledUpLetters) {
          user(id: 4) {
            name
            pets { ... on Badger { name }, ...PetFields }
          }
        }
        fragment PetFields on Peettt {
          name
        }
      """,
      List(
        "Unknown type 'JumbledUpLetters'." -> Some(Pos(2, 25)),
        "Unknown type 'Badger'." -> Some(Pos(5, 27)),
        "Unknown type 'Peettt'." -> Some(Pos(8, 31))
      ))
  }
}
