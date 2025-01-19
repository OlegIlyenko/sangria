package sangria.util

import sangria.ast
import sangria.execution
import sangria.parser.QueryParser
import sangria.schema._
import sangria.validation._
import sangria.util.SimpleGraphQlSupport._

import scala.util.Success
import org.scalatest.matchers.should.Matchers
import sangria.ast.Document
import sangria.util.tag.@@
import sangria.marshalling.FromInput.CoercedScalaResult
import sangria.execution.ExceptionHandler
import sangria.execution.ValueCollector

trait ValidationSupport extends Matchers {
  type TestField = Field[Unit, Unit]

  val Being = InterfaceType(
    "Being",
    List[TestField](
      Field("name", OptionType(StringType), resolve = _ => None)
    ))

  val Pet = InterfaceType(
    "Pet",
    List[TestField](
      Field("name", OptionType(StringType), resolve = _ => None)
    ))

  val Canine = InterfaceType(
    "Canine",
    List[TestField](
      Field(
        "name",
        OptionType(StringType),
        arguments = Argument[Option[Boolean @@ CoercedScalaResult]](
          "surname",
          OptionInputType(BooleanType)) :: Nil,
        resolve = _ => None)
    )
  )

  val DogCommand = EnumType(
    "DogCommand",
    values = List(
      EnumValue("SIT", value = 0),
      EnumValue("HEEL", value = 1),
      EnumValue("DOWN", value = 2)
    ))

  val FurColor = EnumType(
    "FurColor",
    values = List(
      EnumValue("BROWN", value = 0),
      EnumValue("BLACK", value = 1),
      EnumValue("TAN", value = 2),
      EnumValue("SPOTTED", value = 3)
    ))

  val Dog = ObjectType(
    "Dog",
    interfaces[Unit, Unit](Being, Pet, Canine),
    List[TestField](
      Field(
        "name",
        OptionType(StringType),
        arguments = Argument[Option[Boolean @@ CoercedScalaResult]](
          "surname",
          OptionInputType(BooleanType)) :: Nil,
        resolve = _ => None),
      Field("nickname", OptionType(StringType), resolve = _ => None),
      Field("barks", OptionType(BooleanType), resolve = _ => None),
      Field("barkVolume", OptionType(IntType), resolve = _ => None),
      Field(
        "doesKnowCommand",
        OptionType(BooleanType),
        arguments = List(Argument("dogCommand", OptionInputType(DogCommand))),
        resolve = _ => None),
      Field(
        "isHousetrained",
        OptionType(BooleanType),
        arguments = List(Argument("atOtherHomes", OptionInputType(BooleanType), true)),
        resolve = _ => None),
      Field(
        "isAtLocation",
        OptionType(BooleanType),
        arguments =
          List(Argument("x", OptionInputType(IntType)), Argument("y", OptionInputType(IntType))),
        resolve = _ => None
      )
    )
  )

  val Cat = ObjectType(
    "Cat",
    interfaces[Unit, Unit](Being, Pet),
    List[TestField](
      Field("name", OptionType(StringType), resolve = _ => None),
      Field("nickname", OptionType(StringType), resolve = _ => None),
      Field("meows", OptionType(BooleanType), resolve = _ => None),
      Field("meowVolume", OptionType(IntType), resolve = _ => None),
      Field("furColor", OptionType(FurColor), resolve = _ => None)
    )
  )

  val CatOrDog = UnionType("CatOrDog", types = Dog :: Cat :: Nil)

  val Intelligent = InterfaceType(
    "Intelligent",
    List[TestField](
      Field("iq", OptionType(IntType), resolve = _ => None)
    ))

  val Human: ObjectType[Unit, Unit] = ObjectType(
    "Human",
    interfaces[Unit, Unit](Being, Intelligent),
    () =>
      List[TestField](
        Field(
          "name",
          OptionType(StringType),
          arguments = List(Argument("surname", OptionInputType(BooleanType))),
          resolve = _ => None),
        Field("pets", OptionType(ListType(OptionType(Pet))), resolve = _ => None),
        Field("relatives", OptionType(ListType(OptionType(Human))), resolve = _ => None)
      )
  )

  val Alien = ObjectType(
    "Alien",
    interfaces[Unit, Unit](Being, Intelligent),
    List[TestField](Field("numEyes", OptionType(IntType), resolve = _ => None)))

  val DogOrHuman = UnionType("DogOrHuman", types = Dog :: Human :: Nil)

  val HumanOrAlien = UnionType("HumanOrAlien", types = Human :: Alien :: Nil)

  val ComplexInput = InputObjectType(
    "ComplexInput",
    List(
      InputField("requiredField", BooleanType),
      InputField("nonNullField", BooleanType, false),
      InputField("intField", OptionInputType(IntType)),
      InputField("stringField", OptionInputType(StringType)),
      InputField("booleanField", OptionInputType(BooleanType)),
      InputField("stringListField", OptionInputType(ListInputType(OptionInputType(StringType))))
    )
  )

  val OneOfInput = InputObjectType(
    "OneOfInput",
    List(
      InputField("catName", OptionInputType(StringType)),
      InputField("dogId", OptionInputType(IntType))
    )).withDirective(ast.Directive(OneOfDirective.name))

  val ComplicatedArgs = ObjectType(
    "ComplicatedArgs",
    List[TestField](
      Field(
        "intArgField",
        OptionType(StringType),
        arguments = List(Argument("intArg", OptionInputType(IntType))),
        resolve = _ => None),
      Field(
        "bigIntArgField",
        OptionType(StringType),
        arguments = List(Argument("bigIntArg", OptionInputType(BigIntType))),
        resolve = _ => None),
      Field(
        "nonNullIntArgField",
        OptionType(StringType),
        arguments = List(Argument("nonNullIntArg", IntType)),
        resolve = _ => None),
      Field(
        "stringArgField",
        OptionType(StringType),
        arguments = List(Argument("stringArg", OptionInputType(StringType))),
        resolve = _ => None),
      Field(
        "booleanArgField",
        OptionType(StringType),
        arguments = List(Argument("booleanArg", OptionInputType(BooleanType))),
        resolve = _ => None),
      Field(
        "enumArgField",
        OptionType(StringType),
        arguments = List(Argument("enumArg", OptionInputType(FurColor))),
        resolve = _ => None),
      Field(
        "floatArgField",
        OptionType(StringType),
        arguments = List(Argument("floatArg", OptionInputType(FloatType))),
        resolve = _ => None),
      Field(
        "bigDecimalArgField",
        OptionType(StringType),
        arguments = List(Argument("bigDecimalArg", OptionInputType(BigDecimalType))),
        resolve = _ => None),
      Field(
        "idArgField",
        OptionType(StringType),
        arguments = List(Argument("idArg", OptionInputType(IDType))),
        resolve = _ => None),
      Field(
        "stringListArgField",
        OptionType(StringType),
        arguments = List(
          Argument("stringListArg", OptionInputType(ListInputType(OptionInputType(StringType))))),
        resolve = _ => None
      ),
      Field(
        "complexArgField",
        OptionType(StringType),
        arguments = List(Argument("complexArg", OptionInputType(ComplexInput))),
        resolve = _ => None),
      Field(
        "multipleReqs",
        OptionType(StringType),
        arguments = List(Argument("req1", IntType), Argument("req2", IntType)),
        resolve = _ => None),
      Field(
        "nonNullFieldWithDefault",
        OptionType(StringType),
        arguments = List(Argument("arg", IntType, 0)),
        resolve = _ => None),
      Field(
        "multipleOpts",
        OptionType(StringType),
        arguments = List(
          Argument("opt1", OptionInputType(IntType), 0),
          Argument("opt2", OptionInputType(IntType), 0)),
        resolve = _ => None
      ),
      Field(
        "multipleOptAndReq",
        OptionType(StringType),
        arguments = List(
          Argument("req1", IntType),
          Argument("req2", IntType),
          Argument("opt1", OptionInputType(IntType), 0),
          Argument("opt2", OptionInputType(IntType), 0)),
        resolve = _ => None
      )
    )
  )

  val QueryRoot = ObjectType(
    "QueryRoot",
    List[TestField](
      Field("foo", OptionType(StringType), resolve = _ => None),
      Field(
        "human",
        OptionType(Human),
        arguments = List(Argument("id", OptionInputType(IDType))),
        resolve = _ => None),
      Field("alien", OptionType(Alien), resolve = _ => None),
      Field("dog", OptionType(Dog), resolve = _ => None),
      Field("cat", OptionType(Cat), resolve = _ => None),
      Field("pet", OptionType(Pet), resolve = _ => None),
      Field("catOrDog", OptionType(CatOrDog), resolve = _ => None),
      Field("dogOrHuman", OptionType(DogOrHuman), resolve = _ => None),
      Field("humanOrAlien", OptionType(HumanOrAlien), resolve = _ => None),
      Field("complicatedArgs", OptionType(ComplicatedArgs), resolve = _ => None),
      Field(
        "oneOfQuery",
        OptionType(CatOrDog),
        arguments = List(Argument("input", OneOfInput)),
        resolve = _ => None
      )
    )
  )

  private def alwaysInclude(ctx: DirectiveContext): Boolean = true

  val schema = Schema(
    QueryRoot,
    directives = BuiltinDirectives ++ List(
      Directive("onQuery", locations = Set(DirectiveLocation.Query), shouldInclude = alwaysInclude),
      Directive(
        "onMutation",
        locations = Set(DirectiveLocation.Mutation),
        shouldInclude = alwaysInclude),
      Directive(
        "onSubscription",
        locations = Set(DirectiveLocation.Subscription),
        shouldInclude = alwaysInclude),
      Directive("onField", locations = Set(DirectiveLocation.Field), shouldInclude = alwaysInclude),
      Directive(
        "onFragmentDefinition",
        locations = Set(DirectiveLocation.FragmentDefinition),
        shouldInclude = alwaysInclude),
      Directive(
        "onFragmentSpread",
        locations = Set(DirectiveLocation.FragmentSpread),
        shouldInclude = alwaysInclude),
      Directive(
        "onInlineFragment",
        locations = Set(DirectiveLocation.InlineFragment),
        shouldInclude = alwaysInclude),
      Directive(
        "onVariableDefinition",
        locations = Set(DirectiveLocation.VariableDefinition),
        shouldInclude = alwaysInclude),
      Directive(
        "genericDirectiveA",
        locations = Set(DirectiveLocation.FragmentDefinition, DirectiveLocation.Field),
        shouldInclude = alwaysInclude),
      Directive(
        "genericDirectiveB",
        locations = Set(DirectiveLocation.FragmentDefinition, DirectiveLocation.Field),
        shouldInclude = alwaysInclude),
      Directive(
        "repeatableDirective",
        repeatable = true,
        arguments = Argument("id", IntType, "Some generic ID") :: Nil,
        locations = Set(DirectiveLocation.Object),
        shouldInclude = alwaysInclude
      ),
      Directive(
        "onSchema",
        locations = Set(DirectiveLocation.Schema),
        shouldInclude = alwaysInclude),
      Directive(
        "onScalar",
        locations = Set(DirectiveLocation.Scalar),
        shouldInclude = alwaysInclude),
      Directive(
        "onObject",
        locations = Set(DirectiveLocation.Object),
        shouldInclude = alwaysInclude),
      Directive(
        "onFieldDefinition",
        locations = Set(DirectiveLocation.FieldDefinition),
        shouldInclude = alwaysInclude),
      Directive(
        "onArgumentDefinition",
        locations = Set(DirectiveLocation.ArgumentDefinition),
        shouldInclude = alwaysInclude),
      Directive(
        "onInterface",
        locations = Set(DirectiveLocation.Interface),
        shouldInclude = alwaysInclude),
      Directive("onUnion", locations = Set(DirectiveLocation.Union), shouldInclude = alwaysInclude),
      Directive("onEnum", locations = Set(DirectiveLocation.Enum), shouldInclude = alwaysInclude),
      Directive(
        "onEnumValue",
        locations = Set(DirectiveLocation.EnumValue),
        shouldInclude = alwaysInclude),
      Directive(
        "onInputObject",
        locations = Set(DirectiveLocation.InputObject),
        shouldInclude = alwaysInclude),
      Directive(
        "onInputFieldDefinition",
        locations = Set(DirectiveLocation.InputFieldDefinition),
        shouldInclude = alwaysInclude)
    )
  )

  def defaultRule: Option[ValidationRule] = None

  def expectInvalid(
      s: Schema[_, _],
      rules: List[ValidationRule],
      query: String,
      expectedErrors: Seq[(String, Seq[Pos])],
      vars: (String, String) = "" -> ""
  ) = {
    val Success(doc) = QueryParser.parse(query)

    val variables = getVariableValues(s, vars)

    assertViolations(validator(rules).validateQuery(s, doc, variables, None), expectedErrors: _*)
  }

  def expectInputInvalid(
      s: Schema[_, _],
      rules: List[ValidationRule],
      query: String,
      expectedErrors: List[(String, List[Pos])],
      typeName: String) = {
    val Success(doc) = QueryParser.parseInputDocumentWithVariables(query)

    assertViolations(
      validator(rules)
        .validateInputDocument(s, doc, typeName, Map.empty[String, execution.VariableValue]),
      expectedErrors: _*)
  }

  private def getVariableValues(s: Schema[_, _], vars: (String, String)) = {
    import spray.json._
    import sangria.marshalling.sprayJson._

    val valueCollector = new ValueCollector(
      s,
      (if (vars._2.nonEmpty) vars._2 else "{}").parseJson,
      None,
      None,
      (),
      ExceptionHandler.empty,
      None,
      true)

    valueCollector
      .getVariableValues(
        QueryParser
          .parse(s"query Foo${if (vars._1.nonEmpty) "(" + vars._1 + ")" else ""} {foo}")
          .get
          .operations(Some("Foo"))
          .variables,
        None)
      .get
  }

  def expectValid(
      s: Schema[_, _],
      rules: List[ValidationRule],
      query: String,
      vars: (String, String) = "" -> ""
  ) = {
    val Success(doc) = QueryParser.parse(query)

    val variables = getVariableValues(s, vars)

    val errors = validator(rules).validateQuery(s, doc, variables, None)

    withClue(renderViolations(errors)) {
      errors should have size 0
    }
  }

  def expectValidInput(
      s: Schema[_, _],
      rules: List[ValidationRule],
      query: String,
      typeName: String) = {
    val Success(doc) = QueryParser.parseInputDocumentWithVariables(query)

    withClue("Should validate") {
      validator(rules).validateInputDocument(
        s,
        doc,
        typeName,
        Map.empty[String, execution.VariableValue]) should have size 0
    }
  }

  def expectPassesRule(rule: ValidationRule, query: String) =
    expectValid(schema, rule :: Nil, query)

  def expectPasses(
      query: String,
      vars: (String, String) = "" -> ""
  ) =
    expectValid(schema, defaultRule.get :: Nil, query, vars)

  def expectInputPasses(typeName: String, query: String) =
    expectValidInput(schema, defaultRule.get :: Nil, query, typeName)

  def expectFailsRule(
      rule: ValidationRule,
      query: String,
      expectedErrors: List[(String, Option[Pos])]) =
    expectInvalid(
      schema,
      rule :: Nil,
      query,
      expectedErrors.map { case (msg, pos) => msg -> pos.toList })

  def expectFails(
      query: String,
      expectedErrors: List[(String, Option[Pos])],
      vars: (String, String) = "" -> ""
  ) =
    expectInvalid(
      schema,
      defaultRule.get :: Nil,
      query,
      expectedErrors.map { case (msg, pos) => msg -> pos.toList },
      vars
    )

  def expectInputFails(typeName: String, query: String, expectedErrors: List[(String, List[Pos])]) =
    expectInputInvalid(schema, defaultRule.get :: Nil, query, expectedErrors, typeName)

  def expectFailsPosList(query: String, expectedErrors: List[(String, List[Pos])]) =
    expectInvalid(schema, defaultRule.get :: Nil, query, expectedErrors)

  def expectFailsSimple(query: String, expectedErrors: (String, Seq[Pos])*) =
    expectInvalid(schema, defaultRule.get :: Nil, query, expectedErrors)

  def expectFailsSDL(initialSchemaSDL: String, sdlUnderTest: String, v: Option[ValidationRule])(
      violationCheck: Violation => Unit): Unit = {
    val Success(doc) = QueryParser.parse(initialSchemaSDL)

    expectFailsSDL(doc.merge(Document.emptyStub), sdlUnderTest, v)(violationCheck)
  }

  def expectFailsSDL(initialSchemaDoc: Document, sdlUnderTest: String, v: Option[ValidationRule])(
      violationCheck: Violation => Unit): Unit = {
    val schema = Schema.buildFromAst(initialSchemaDoc)
    val Success(docUnderTest) = QueryParser.parse(sdlUnderTest)
    val violations = validator(v.toList).validateQuery(
      schema,
      docUnderTest,
      Map.empty[String, execution.VariableValue],
      None)
    violations shouldNot be(empty)
    violations.size shouldBe 1
    violationCheck(violations.head)
  }

  def expectPassesSDL(
      initialSchemaSDL: String,
      sdlUnderTest: String,
      v: Option[ValidationRule]): Unit = {
    val Success(doc) = QueryParser.parse(initialSchemaSDL)
    expectPassesSDL(doc.merge(Document.emptyStub), sdlUnderTest, v)
  }
  def expectPassesSDL(
      initialSchemaDoc: Document,
      sdlUnderTest: String,
      v: Option[ValidationRule]): Unit = {
    val schema = Schema.buildFromAst(initialSchemaDoc)
    val Success(docUnderTest) = QueryParser.parse(sdlUnderTest)
    val violations = validator(v.toList).validateQuery(
      schema,
      docUnderTest,
      Map.empty[String, execution.VariableValue],
      None)
    violations shouldBe empty
  }

  def validator(rules: List[ValidationRule]) = new RuleBasedQueryValidator(rules)
}
