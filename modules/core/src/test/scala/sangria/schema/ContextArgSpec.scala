package sangria.schema

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import sangria.util.SimpleGraphQlSupport._

class ContextArgSpec extends AnyWordSpec with Matchers {
  private val OptionalStringArg: Argument[Option[String]] =
    Argument("fooArg", OptionInputType(StringType))

  private val QueryType = ObjectType(
    "Query",
    fields[Unit, Unit](
      Field(
        "foo",
        StringType,
        arguments = OptionalStringArg :: Nil,
        resolve = ctx => ctx.arg(OptionalStringArg).toString),
      Field(
        "fooOpt",
        StringType,
        arguments = OptionalStringArg :: Nil,
        resolve = ctx => ctx.argOpt(OptionalStringArg).toString)
    )
  )

  private val schema = Schema(QueryType)

  "Context.arg" should {
    "return Some(...) if the field have the argument and a non-null value" in
      check(
        schema,
        (),
        """
          {
            foo(fooArg: "hello")
          }
        """,
        Map("data" -> Map("foo" -> "Some(hello)")))
    "return None if the field have the argument but no value" in
      check(
        schema,
        (),
        """
          {
            foo(fooArg: null)
          }
        """,
        Map("data" -> Map("foo" -> "None")))
    "return None if the field does not have the argument" in
      check(
        schema,
        (),
        """
          {
            foo
          }
        """,
        Map("data" -> Map("foo" -> "None")))
  }

  "Context.argOpt" should {
    "return Some(Some(...)) if the field have the argument and a non-null value" in
      check(
        schema,
        (),
        """
          {
            fooOpt(fooArg: "hello")
          }
        """,
        Map("data" -> Map("fooOpt" -> "Some(Some(hello))")))
    "return Some(None) if the field have the argument but no value" in
      check(
        schema,
        (),
        """
          {
            fooOpt(fooArg: null)
          }
        """,
        Map("data" -> Map("fooOpt" -> "Some(None)")))
    "return None if the field does not have the argument" in
      check(
        schema,
        (),
        """
          {
            fooOpt
          }
        """,
        Map("data" -> Map("fooOpt" -> "None")))
  }
}
