package sangria.schema

import sangria.ast.Document
import sangria.execution.Executor
import sangria.macros._
import sangria.util.FutureResultSupport
import spray.json.JsValue

import scala.concurrent.ExecutionContext.Implicits.global

import spray.json._
import sangria.marshalling.sprayJson._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import sangria.util.tag.@@ // Scala 3 issue workaround
import sangria.marshalling.FromInput.CoercedScalaResult

class DefaultValueApplicationSpec extends AnyWordSpec with Matchers with FutureResultSupport {
  "Default value application" should {
    "use default value if argument is not provided" in {
      val assertion = (args: Args) =>
        args.withArgs(AArg, SizeArg) { (test, size) =>
          test should be("default")
          size should be(42)

          args.argDefinedInQuery(AArg) should be(false)
          args.argOpt(AArg) should be(Some("default"))
        }

      check(graphql"{ test }", "{}", assertion)
      check(graphql"query ($$x: String) { test(a: $$x) }", "{}", assertion)
    }

    "use default value if argument is `null`" in {
      val assertion = (args: Args) =>
        args.withArgs(AArg) { test =>
          test should be("default")

          args.argOpt(AArg) should be(None)
          args.argDefinedInQuery(AArg) should be(true)
        }

      check(graphql"{ test(a: null) }", "{}", assertion)
      check(graphql"query ($$x: String = null) { test(a: $$x) }", "{}", assertion)
      check(graphql"query ($$x: String) { test(a: $$x) }", """{"x": null}""", assertion)
    }

    "not use default value if value is provided" in {
      val assertion = (args: Args) =>
        args.withArgs(AArg) { test =>
          test should be("bar")

          args.argOpt(AArg) should be(Some("bar"))
          args.argDefinedInQuery(AArg) should be(true)
        }

      check(graphql"""{ test(a: "bar") }""", "{}", assertion)
      check(graphql"query ($$x: String) { test(a: $$x) }", """{"x": "bar"}""", assertion)
    }

    "default variable value should not be used if variable is set to `null`" in
      check(
        graphql"""query ($$x: String = "bar") { test(a: $$x) }""",
        """{"x": null}""",
        args => {
          args.arg(AArg) should be("bar")

          args.argOpt(AArg) should be(None)
          args.argDefinedInQuery(AArg) should be(true)
        }
      )

    "default variable value should not be used if variable is not set" in
      check(
        graphql"""query ($$x: String = "bar") { test(a: $$x) }""",
        """{}""",
        args => {
          args.arg(AArg) should be("bar")

          args.argOpt(AArg) should be(Some("bar"))
          args.argDefinedInQuery(AArg) should be(true)
        }
      )

    "use default value if field is not provided" in {
      val assertion = (args: Args) =>
        args.withArgs(InpArg, InpJsonArg) { (inp, inpJson) =>
          inp should be(Map("f" -> Some("default")))
          inpJson should be("""{"f": "default"}""".parseJson)
        }

      check(graphql"{ testInp(inp: {}, inpJson: {}) }", "{}", assertion)
      check(
        graphql"query ($$x: String) { testInp(inp: {f: $$x}, inpJson: {f: $$x}) }",
        "{}",
        assertion)
    }

    "not use default value if field is `null`" in {
      val assertion = (args: Args) =>
        args.withArgs(InpArg, InpJsonArg) { (inp, inpJson) =>
          inp should be(Map("f" -> None))
          inpJson should be("""{"f": null}""".parseJson)
        }

      check(graphql"{ testInp(inp: {f: null}, inpJson: {f: null}) }", "{}", assertion)
      check(
        graphql"query ($$x: String = null) { testInp(inp: {f: $$x}, inpJson: {f: $$x}) }",
        "{}",
        assertion)
      check(
        graphql"query ($$x: String) { testInp(inp: {f: $$x}, inpJson: {f: $$x}) }",
        """{"x": null}""",
        assertion)
    }

    "not use default value if field value is provided" in {
      val assertion = (args: Args) =>
        args.withArgs(InpArg, InpJsonArg) { (inp, inpJson) =>
          inp should be(Map("f" -> Some("bar")))
          inpJson should be("""{"f": "bar"}""".parseJson)
        }

      check(graphql"""{ testInp(inp: {f: "bar"}, inpJson: {f: "bar"}) }""", "{}", assertion)
      check(
        graphql"query ($$x: String) { testInp(inp: {f: $$x}, inpJson: {f: $$x}) }",
        """{"x": "bar"}""",
        assertion)
    }

    "default variable value should not be used if variable is set to `null` for a field" in
      check(
        graphql"""query ($$x: String = "bar") { testInp(inp: {f: $$x}, inpJson: {f: $$x}) }""",
        """{"x": null}""",
        args =>
          args.withArgs(InpArg, InpJsonArg) { (inp, inpJson) =>
            inp should be(Map("f" -> None))
            inpJson should be("""{"f": null}""".parseJson)
          }
      )

    "default variable value should not be used if variable is not set for a field" in
      check(
        graphql"""query ($$x: String = "bar") { testInp(inp: {f: $$x}, inpJson: {f: $$x}) }""",
        """{}""",
        args =>
          args.withArgs(InpArg, InpJsonArg) { (inp, inpJson) =>
            inp should be(Map("f" -> Some("bar")))
            inpJson should be("""{"f": "bar"}""".parseJson)
          }
      )

    "set fields to `null` if value is not set, but default is set to null" in
      check(
        graphql"""query ($$x: String = "bar", $$y: String = null) { testInp(inp: {f: $$x, fo: $$y}, inpJson: {f: $$x, fo: $$y}) }""",
        """{}""",
        args =>
          args.withArgs(InpArg, InpJsonArg) { (inp, inpJson) =>
            inp should be(Map("f" -> Some("bar"), "fo" -> None))
            inpJson should be("""{"f": "bar", "fo": null}""".parseJson)
          }
      )
  }

  class Ctx {
    var args: Option[Args] = None
  }

  private[this] val TestInputJsonType = InputObjectType[JsValue](
    "TestInput",
    fields = List(
      InputField("f", OptionInputType(StringType), defaultValue = "default"),
      InputField("fo", OptionInputType(StringType))))

  private[this] val TestInputType = InputObjectType(
    "TestInput",
    fields = List(
      InputField("f", OptionInputType(StringType), defaultValue = "default"),
      InputField("fo", OptionInputType(StringType))))

  private[this] val AArg = Argument[Option[String @@ CoercedScalaResult], String](
    "a",
    OptionInputType(StringType),
    defaultValue = "default")
  private[this] val InpArg = Argument("inp", TestInputType)
  private[this] val InpJsonArg = Argument("inpJson", TestInputJsonType)
  private[this] val SizeArg =
    Argument[Option[Int @@ CoercedScalaResult], Int]("size", OptionInputType(IntType), 42)

  private[this] val QueryType = ObjectType(
    "Query",
    fields[Ctx, Unit](
      Field(
        "test",
        StringType,
        arguments = AArg :: SizeArg :: Nil,
        resolve = c => {
          c.ctx.args = Some(c.args)
          "foo"
        }),
      Field(
        "testInp",
        StringType,
        arguments = InpArg :: InpJsonArg :: Nil,
        resolve = c => {
          c.ctx.args = Some(c.args)
          "foo"
        })
    )
  )

  private[this] val schema = Schema(QueryType)

  private[this] def check[T](query: Document, vars: String, assertions: Args => T): T = {
    val ctx = new Ctx

    Executor.execute(schema, query, ctx, variables = vars.parseJson).await

    assertions(ctx.args.getOrElse(fail("field was not used in query!")))
  }
}
