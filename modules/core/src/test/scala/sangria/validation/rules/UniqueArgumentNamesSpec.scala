package sangria.validation.rules

import sangria.util.{Pos, ValidationSupport}
import org.scalatest.wordspec.AnyWordSpec

class UniqueArgumentNamesSpec extends AnyWordSpec with ValidationSupport {

  override val defaultRule = Some(new UniqueArgumentNames)

  "Validate: Unique argument names" should {
    "no arguments on field" in expectPasses("""
        {
          field
        }
      """)

    "no arguments on directive" in expectPasses("""
        {
          field
        }
      """)

    "argument on field" in expectPasses("""
        {
          field(arg: "value")
        }
      """)

    "argument on directive" in expectPasses("""
        {
          field @directive(arg: "value")
        }
      """)

    "same argument on two fields" in expectPasses("""
        {
          one: field(arg: "value")
          two: field(arg: "value")
        }
      """)

    "same argument on field and directive" in expectPasses("""
        {
          field(arg: "value") @directive(arg: "value")
        }
      """)

    "same argument on two directives" in expectPasses("""
        {
          field @directive1(arg: "value") @directive2(arg: "value")
        }
      """)

    "multiple field arguments" in expectPasses("""
        {
          field(arg1: "value", arg2: "value", arg3: "value")
        }
      """)

    "multiple directive arguments" in expectPasses("""
        {
          field @directive(arg1: "value", arg2: "value", arg3: "value")
        }
      """)

    "duplicate field arguments" in expectFails(
      """
        {
          field(arg1: "value", arg1: "value")
        }
      """,
      List(
        "There can be only one argument named 'arg1'." -> Some(Pos(3, 32))
      )
    )

    "many duplicate field arguments" in expectFails(
      """
        {
          field(arg1: "value", arg1: "value", arg1: "value")
        }
      """,
      List(
        "There can be only one argument named 'arg1'." -> Some(Pos(3, 32)),
        "There can be only one argument named 'arg1'." -> Some(Pos(3, 47))
      )
    )

    "duplicate directive arguments" in expectFails(
      """
        {
          field @directive(arg1: "value", arg1: "value")
        }
      """,
      List(
        "There can be only one argument named 'arg1'." -> Some(Pos(3, 43))
      )
    )

    "many duplicate directive arguments" in expectFails(
      """
        {
          field @directive(arg1: "value", arg1: "value", arg1: "value")
        }
      """,
      List(
        "There can be only one argument named 'arg1'." -> Some(Pos(3, 43)),
        "There can be only one argument named 'arg1'." -> Some(Pos(3, 58))
      )
    )
  }
}
