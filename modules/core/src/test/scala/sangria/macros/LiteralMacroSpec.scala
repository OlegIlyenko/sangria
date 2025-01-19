package sangria.macros

import sangria.ast.AstLocation
import sangria.ast._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import sangria.schema.AstNodeTransformer

class LiteralMacroSpec extends AnyWordSpec with Matchers {
  "literal graphql macro" should {
    "fail compilation on syntax error" in {
      """
        val ast = graphql" query Foo { luke...on 111 Fragment} "
      """ shouldNot compile
    }

    "parse complex query" in {
      val ast =
        graphql"""
          # test query
          query FetchLukeAndLeiaAliased($$someVar: Int = 1.23,$$anotherVar: Int = 123)@include(if: true) @include(if: false){
            luke: human(id: "1000")@include(if: true){
              friends(sort: NAME)
            }
            leia: human(id , : , "10103\n \u00F6 ö") {
              name # some name
            }

            ... on User {
              birth{day}
            }

            ...Foo
          }

          fragment Foo on User @foo(bar: 1){
            baz # field in fragment!
          }
        """

      ast.sourceMapper should not be Symbol("empty")

      AstNodeTransformer.withoutAstLocations(ast.withoutSourceMapper) should be(
        Document(
          Vector(
            OperationDefinition(
              OperationType.Query,
              Some("FetchLukeAndLeiaAliased"),
              Vector(
                VariableDefinition(
                  "someVar",
                  NamedType("Int", None),
                  Some(BigDecimalValue(1.23, Vector.empty, None)),
                  Vector.empty,
                  Vector.empty,
                  None
                ),
                VariableDefinition(
                  "anotherVar",
                  NamedType("Int", None),
                  Some(BigIntValue(123, Vector.empty, None)),
                  Vector.empty,
                  Vector.empty,
                  None
                )
              ),
              Vector(
                Directive(
                  "include",
                  Vector(
                    Argument(
                      "if",
                      BooleanValue(true, Vector.empty, None),
                      Vector.empty,
                      None
                    )),
                  Vector.empty,
                  None
                ),
                Directive(
                  "include",
                  Vector(
                    Argument(
                      "if",
                      BooleanValue(false, Vector.empty, None),
                      Vector.empty,
                      None
                    )),
                  Vector.empty,
                  None
                )
              ),
              Vector(
                Field(
                  Some("luke"),
                  "human",
                  Vector(
                    Argument(
                      "id",
                      StringValue("1000", false, None, Vector.empty, None),
                      Vector.empty,
                      None
                    )),
                  Vector(
                    Directive(
                      "include",
                      Vector(
                        Argument(
                          "if",
                          BooleanValue(true, Vector.empty, None),
                          Vector.empty,
                          None
                        )),
                      Vector.empty,
                      None
                    )),
                  Vector(Field(
                    None,
                    "friends",
                    Vector(
                      Argument(
                        "sort",
                        EnumValue("NAME", Vector.empty, None),
                        Vector.empty,
                        None
                      )),
                    Vector.empty,
                    Vector.empty,
                    Vector.empty,
                    Vector.empty,
                    None
                  )),
                  Vector.empty,
                  Vector.empty,
                  None
                ),
                Field(
                  Some("leia"),
                  "human",
                  Vector(
                    Argument(
                      "id",
                      StringValue("10103\n \u00F6 \u00F6", false, None, Vector.empty, None),
                      Vector.empty,
                      None
                    )),
                  Vector.empty,
                  Vector(
                    Field(
                      None,
                      "name",
                      Vector.empty,
                      Vector.empty,
                      Vector.empty,
                      Vector.empty,
                      Vector.empty,
                      None
                    )),
                  Vector.empty,
                  Vector(Comment(" some name", None)),
                  None
                ),
                InlineFragment(
                  Some(NamedType("User", None)),
                  Vector.empty,
                  Vector(Field(
                    None,
                    "birth",
                    Vector.empty,
                    Vector.empty,
                    Vector(
                      Field(
                        None,
                        "day",
                        Vector.empty,
                        Vector.empty,
                        Vector.empty,
                        Vector.empty,
                        Vector.empty,
                        None
                      )),
                    Vector.empty,
                    Vector.empty,
                    None
                  )),
                  Vector.empty,
                  Vector.empty,
                  None
                ),
                FragmentSpread("Foo", Vector.empty, Vector.empty, None)
              ),
              Vector(Comment(" test query", None)),
              Vector.empty,
              None
            ),
            FragmentDefinition(
              "Foo",
              NamedType("User", None),
              Vector(
                Directive(
                  "foo",
                  Vector(
                    Argument(
                      "bar",
                      BigIntValue(1, Vector.empty, None),
                      Vector.empty,
                      None
                    )),
                  Vector.empty,
                  None
                )),
              Vector(
                Field(
                  None,
                  "baz",
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  None
                )),
              Vector.empty,
              Vector.empty,
              Vector(Comment(" field in fragment!", None)),
              None
            )
          ),
          Vector.empty,
          None,
          None
        ))
    }

    "parse kitchen sink" in {
      val ast =
        graphql"""
          # Copyright (c) 2015, Facebook, Inc.
          # All rights reserved.
          #
          # This source code is licensed under the BSD-style license found in the
          # LICENSE file in the root directory of this source tree. An additional grant
          # of patent rights can be found in the PATENTS file in the same directory.

          query queryName($$foo: ComplexType, $$site: Site = MOBILE) {
            whoever123is: node(id: [123, 456]) {
              id ,
              ... on User @defer {
                field2 {
                  id ,
                  alias: field1(first:10, after:$$foo,) @include(if: $$foo) {
                    id,
                    ...frag
                  }
                }
              }
            }
          }

          subscription StoryLikeSubscription($$input: StoryLikeSubscribeInput) {
            storyLikeSubscribe(input: $$input) {
              story {
                likers {
                  count
                }
                likeSentence {
                  text
                }
              }
            }
          }

          mutation likeStory {
            like(story: 123) @defer {
              story {
                id
              }
            }
          }

          fragment frag on Friend {
            foo(size: $$size, bar: $$b, obj: {key: "value"})
          }

          {
            unnamed(truthy: true, falsey: false),
            query
          }
        """

      ast.sourceMapper should not be Symbol("empty")

      AstNodeTransformer.withoutAstLocations(ast.withoutSourceMapper) should be(
        Document(
          Vector(
            OperationDefinition(
              OperationType.Query,
              Some("queryName"),
              Vector(
                VariableDefinition(
                  "foo",
                  NamedType("ComplexType", None),
                  None,
                  Vector.empty,
                  Vector.empty,
                  None
                ),
                VariableDefinition(
                  "site",
                  NamedType("Site", None),
                  Some(EnumValue("MOBILE", Vector.empty, None)),
                  Vector.empty,
                  Vector.empty,
                  None
                )
              ),
              Vector.empty,
              Vector(
                Field(
                  Some("whoever123is"),
                  "node",
                  Vector(
                    Argument(
                      "id",
                      ListValue(
                        Vector(
                          BigIntValue(123, Vector.empty, None),
                          BigIntValue(456, Vector.empty, None)),
                        Vector.empty,
                        None
                      ),
                      Vector.empty,
                      None
                    )),
                  Vector.empty,
                  Vector(
                    Field(
                      None,
                      "id",
                      Vector.empty,
                      Vector.empty,
                      Vector.empty,
                      Vector.empty,
                      Vector.empty,
                      None
                    ),
                    InlineFragment(
                      Some(NamedType("User", None)),
                      Vector(
                        Directive(
                          "defer",
                          Vector.empty,
                          Vector.empty,
                          None
                        )),
                      Vector(
                        Field(
                          None,
                          "field2",
                          Vector.empty,
                          Vector.empty,
                          Vector(
                            Field(
                              None,
                              "id",
                              Vector.empty,
                              Vector.empty,
                              Vector.empty,
                              Vector.empty,
                              Vector.empty,
                              None
                            ),
                            Field(
                              Some("alias"),
                              "field1",
                              Vector(
                                Argument(
                                  "first",
                                  BigIntValue(10, Vector.empty, None),
                                  Vector.empty,
                                  None
                                ),
                                Argument(
                                  "after",
                                  VariableValue("foo", Vector.empty, None),
                                  Vector.empty,
                                  None
                                )),
                              Vector(
                                Directive(
                                  "include",
                                  Vector(
                                    Argument(
                                      "if",
                                      VariableValue("foo", Vector.empty, None),
                                      Vector.empty,
                                      None
                                    )),
                                  Vector.empty,
                                  None
                                )),
                              Vector(
                                Field(
                                  None,
                                  "id",
                                  Vector.empty,
                                  Vector.empty,
                                  Vector.empty,
                                  Vector.empty,
                                  Vector.empty,
                                  None
                                ),
                                FragmentSpread("frag", Vector.empty, Vector.empty, None)),
                              Vector.empty,
                              Vector.empty,
                              None
                            )
                          ),
                          Vector.empty,
                          Vector.empty,
                          None
                        )),
                      Vector.empty,
                      Vector.empty,
                      None
                    )
                  ),
                  Vector.empty,
                  Vector.empty,
                  None
                )),
              Vector(
                Comment(" Copyright (c) 2015, Facebook, Inc.", None),
                Comment(" All rights reserved.", None),
                Comment("", None),
                Comment(
                  " This source code is licensed under the BSD-style license found in the",
                  None),
                Comment(
                  " LICENSE file in the root directory of this source tree. An additional grant",
                  None),
                Comment(
                  " of patent rights can be found in the PATENTS file in the same directory.",
                  None)
              ),
              Vector.empty,
              None
            ),
            OperationDefinition(
              OperationType.Subscription,
              Some("StoryLikeSubscription"),
              Vector(
                VariableDefinition(
                  "input",
                  NamedType("StoryLikeSubscribeInput", None),
                  None,
                  Vector.empty,
                  Vector.empty,
                  None
                )),
              Vector.empty,
              Vector(
                Field(
                  None,
                  "storyLikeSubscribe",
                  Vector(
                    Argument(
                      "input",
                      VariableValue("input", Vector.empty, None),
                      Vector.empty,
                      None
                    )),
                  Vector.empty,
                  Vector(
                    Field(
                      None,
                      "story",
                      Vector.empty,
                      Vector.empty,
                      Vector(
                        Field(
                          None,
                          "likers",
                          Vector.empty,
                          Vector.empty,
                          Vector(
                            Field(
                              None,
                              "count",
                              Vector.empty,
                              Vector.empty,
                              Vector.empty,
                              Vector.empty,
                              Vector.empty,
                              None
                            )),
                          Vector.empty,
                          Vector.empty,
                          None
                        ),
                        Field(
                          None,
                          "likeSentence",
                          Vector.empty,
                          Vector.empty,
                          Vector(
                            Field(
                              None,
                              "text",
                              Vector.empty,
                              Vector.empty,
                              Vector.empty,
                              Vector.empty,
                              Vector.empty,
                              None
                            )),
                          Vector.empty,
                          Vector.empty,
                          None
                        )
                      ),
                      Vector.empty,
                      Vector.empty,
                      None
                    )),
                  Vector.empty,
                  Vector.empty,
                  None
                )),
              Vector.empty,
              Vector.empty,
              None
            ),
            OperationDefinition(
              OperationType.Mutation,
              Some("likeStory"),
              Vector.empty,
              Vector.empty,
              Vector(
                Field(
                  None,
                  "like",
                  Vector(
                    Argument(
                      "story",
                      BigIntValue(123, Vector.empty, None),
                      Vector.empty,
                      None
                    )),
                  Vector(
                    Directive(
                      "defer",
                      Vector.empty,
                      Vector.empty,
                      None
                    )),
                  Vector(Field(
                    None,
                    "story",
                    Vector.empty,
                    Vector.empty,
                    Vector(
                      Field(
                        None,
                        "id",
                        Vector.empty,
                        Vector.empty,
                        Vector.empty,
                        Vector.empty,
                        Vector.empty,
                        None
                      )),
                    Vector.empty,
                    Vector.empty,
                    None
                  )),
                  Vector.empty,
                  Vector.empty,
                  None
                )),
              Vector.empty,
              Vector.empty,
              None
            ),
            FragmentDefinition(
              "frag",
              NamedType("Friend", None),
              Vector.empty,
              Vector(
                Field(
                  None,
                  "foo",
                  Vector(
                    Argument(
                      "size",
                      VariableValue("size", Vector.empty, None),
                      Vector.empty,
                      None
                    ),
                    Argument(
                      "bar",
                      VariableValue("b", Vector.empty, None),
                      Vector.empty,
                      None
                    ),
                    Argument(
                      "obj",
                      ObjectValue(
                        Vector(
                          ObjectField(
                            "key",
                            StringValue("value", false, None, Vector.empty, None),
                            Vector.empty,
                            None
                          )),
                        Vector.empty,
                        None
                      ),
                      Vector.empty,
                      None
                    )
                  ),
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  None
                )),
              Vector.empty,
              Vector.empty,
              Vector.empty,
              None
            ),
            OperationDefinition(
              OperationType.Query,
              None,
              Vector.empty,
              Vector.empty,
              Vector(
                Field(
                  None,
                  "unnamed",
                  Vector(
                    Argument(
                      "truthy",
                      BooleanValue(true, Vector.empty, None),
                      Vector.empty,
                      None
                    ),
                    Argument(
                      "falsey",
                      BooleanValue(false, Vector.empty, None),
                      Vector.empty,
                      None
                    )
                  ),
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  None
                ),
                Field(
                  None,
                  "query",
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  None
                )
              ),
              Vector.empty,
              Vector.empty,
              None
            )
          ),
          Vector.empty,
          None,
          None
        )
      )
    }

    "parse schema kitchen sink" in {
      val ast =
        graphql"""
          # Copyright (c) 2015, Facebook, Inc.
          # All rights reserved.
          #
          # This source code is licensed under the BSD-style license found in the
          # LICENSE file in the root directory of this source tree. An additional grant
          # of patent rights can be found in the PATENTS file in the same directory.

          schema {
            query: QueryType
            mutation: MutationType
          }

          "Type description *test*"
          type Foo implements Bar {
            one: Type
            "another description"
            two(argument: InputType!): Type
            three(argument: InputType, other: String): Int
            four(argument: String = "string"): String
            five(argument: [String] = ["string", "string"]): String
            six(argument: InputType = {key: "value"}): Type
          }

          type AnnotatedObject @onObject(arg: "value") {
            annotatedField(arg: Type = "default" @onArg): Type @onField
          }

          # It's an interface!
          interface Bar {
            one: Type
            four(argument: String = "string"): String
          }

          interface AnnotatedInterface @onInterface {
            annotatedField(arg: Type @onArg): Type @onField
          }

          union Feed = Story | Article | Advert

          union AnnotatedUnion @onUnion = A | B

          scalar CustomScalar

          scalar AnnotatedScalar @onScalar

          enum Site {
            # value 1
            DESKTOP
            # value 2
            MOBILE
          }

          enum AnnotatedEnum @onEnum {
            ANNOTATED_VALUE @onEnumValue
            OTHER_VALUE
          }

          input InputType {
            key: String!
            answer: Int = 42
          }

          input AnnotatedInput @onInputObjectType {
            # field comment
            annotatedField: Type @onField
          }

          extend type Foo {
            seven(argument: [String]): Type
          }

          extend type Foo @onType {
            foo: String
          }

          directive @skip(if: Boolean!) on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT

          directive @include(if: Boolean!)
            on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT

        """

      ast.sourceMapper should not be Symbol("empty")

      AstNodeTransformer.withoutAstLocations(ast.withoutSourceMapper) should be(
        Document(
          Vector(
            SchemaDefinition(
              Vector(
                OperationTypeDefinition(
                  OperationType.Query,
                  NamedType("QueryType", None),
                  Vector.empty,
                  None),
                OperationTypeDefinition(
                  OperationType.Mutation,
                  NamedType("MutationType", None),
                  Vector.empty,
                  None)
              ),
              Vector.empty,
              None,
              Vector(
                Comment(" Copyright (c) 2015, Facebook, Inc.", None),
                Comment(" All rights reserved.", None),
                Comment("", None),
                Comment(
                  " This source code is licensed under the BSD-style license found in the",
                  None),
                Comment(
                  " LICENSE file in the root directory of this source tree. An additional grant",
                  None),
                Comment(
                  " of patent rights can be found in the PATENTS file in the same directory.",
                  None)
              ),
              Vector.empty,
              None
            ),
            ObjectTypeDefinition(
              "Foo",
              Vector(NamedType("Bar", None)),
              Vector(
                FieldDefinition(
                  "one",
                  NamedType("Type", None),
                  Vector.empty,
                  Vector.empty,
                  None,
                  Vector.empty,
                  None),
                FieldDefinition(
                  "two",
                  NamedType("Type", None),
                  Vector(
                    InputValueDefinition(
                      "argument",
                      NotNullType(NamedType("InputType", None), None),
                      None,
                      Vector.empty,
                      None,
                      Vector.empty,
                      None)),
                  Vector.empty,
                  Some(StringValue("another description", false, None, Vector.empty, None)),
                  Vector.empty,
                  None
                ),
                FieldDefinition(
                  "three",
                  NamedType("Int", None),
                  Vector(
                    InputValueDefinition(
                      "argument",
                      NamedType("InputType", None),
                      None,
                      Vector.empty,
                      None,
                      Vector.empty,
                      None),
                    InputValueDefinition(
                      "other",
                      NamedType("String", None),
                      None,
                      Vector.empty,
                      None,
                      Vector.empty,
                      None)
                  ),
                  Vector.empty,
                  None,
                  Vector.empty,
                  None
                ),
                FieldDefinition(
                  "four",
                  NamedType("String", None),
                  Vector(
                    InputValueDefinition(
                      "argument",
                      NamedType("String", None),
                      Some(StringValue("string", false, None, Vector.empty, None)),
                      Vector.empty,
                      None,
                      Vector.empty,
                      None)),
                  Vector.empty,
                  None,
                  Vector.empty,
                  None
                ),
                FieldDefinition(
                  "five",
                  NamedType("String", None),
                  Vector(InputValueDefinition(
                    "argument",
                    ListType(NamedType("String", None), None),
                    Some(
                      ListValue(
                        Vector(
                          StringValue("string", false, None, Vector.empty, None),
                          StringValue("string", false, None, Vector.empty, None)),
                        Vector.empty,
                        None
                      )),
                    Vector.empty,
                    None,
                    Vector.empty,
                    None
                  )),
                  Vector.empty,
                  None,
                  Vector.empty,
                  None
                ),
                FieldDefinition(
                  "six",
                  NamedType("Type", None),
                  Vector(InputValueDefinition(
                    "argument",
                    NamedType("InputType", None),
                    Some(
                      ObjectValue(
                        Vector(ObjectField(
                          "key",
                          StringValue("value", false, None, Vector.empty, None),
                          Vector.empty,
                          None
                        )),
                        Vector.empty,
                        None
                      )),
                    Vector.empty,
                    None,
                    Vector.empty,
                    None
                  )),
                  Vector.empty,
                  None,
                  Vector.empty,
                  None
                )
              ),
              Vector.empty,
              Some(StringValue("Type description *test*", false, None, Vector.empty, None)),
              Vector.empty,
              Vector.empty,
              None
            ),
            ObjectTypeDefinition(
              "AnnotatedObject",
              Vector.empty,
              Vector(
                FieldDefinition(
                  "annotatedField",
                  NamedType("Type", None),
                  Vector(InputValueDefinition(
                    "arg",
                    NamedType("Type", None),
                    Some(StringValue("default", false, None, Vector.empty, None)),
                    Vector(
                      Directive(
                        "onArg",
                        Vector.empty,
                        Vector.empty,
                        None
                      )),
                    None,
                    Vector.empty,
                    None
                  )),
                  Vector(
                    Directive(
                      "onField",
                      Vector.empty,
                      Vector.empty,
                      None
                    )),
                  None,
                  Vector.empty,
                  None
                )),
              Vector(
                Directive(
                  "onObject",
                  Vector(
                    Argument(
                      "arg",
                      StringValue("value", false, None, Vector.empty, None),
                      Vector.empty,
                      None
                    )),
                  Vector.empty,
                  None
                )),
              None,
              Vector.empty,
              Vector.empty,
              None
            ),
            InterfaceTypeDefinition(
              "Bar",
              Vector(
                FieldDefinition(
                  "one",
                  NamedType("Type", None),
                  Vector.empty,
                  Vector.empty,
                  None,
                  Vector.empty,
                  None),
                FieldDefinition(
                  "four",
                  NamedType("String", None),
                  Vector(
                    InputValueDefinition(
                      "argument",
                      NamedType("String", None),
                      Some(StringValue("string", false, None, Vector.empty, None)),
                      Vector.empty,
                      None,
                      Vector.empty,
                      None)),
                  Vector.empty,
                  None,
                  Vector.empty,
                  None
                )
              ),
              Vector.empty,
              None,
              Vector(Comment(" It's an interface!", None)),
              Vector.empty,
              None
            ),
            InterfaceTypeDefinition(
              "AnnotatedInterface",
              Vector(FieldDefinition(
                "annotatedField",
                NamedType("Type", None),
                Vector(
                  InputValueDefinition(
                    "arg",
                    NamedType("Type", None),
                    None,
                    Vector(
                      Directive(
                        "onArg",
                        Vector.empty,
                        Vector.empty,
                        None
                      )),
                    None,
                    Vector.empty,
                    None)),
                Vector(
                  Directive(
                    "onField",
                    Vector.empty,
                    Vector.empty,
                    None
                  )),
                None,
                Vector.empty,
                None
              )),
              Vector(
                Directive(
                  "onInterface",
                  Vector.empty,
                  Vector.empty,
                  None
                )),
              None,
              Vector.empty,
              Vector.empty,
              None
            ),
            UnionTypeDefinition(
              "Feed",
              Vector(
                NamedType("Story", None),
                NamedType("Article", None),
                NamedType("Advert", None)),
              Vector.empty,
              None,
              Vector.empty,
              None
            ),
            UnionTypeDefinition(
              "AnnotatedUnion",
              Vector(NamedType("A", None), NamedType("B", None)),
              Vector(
                Directive(
                  "onUnion",
                  Vector.empty,
                  Vector.empty,
                  None
                )),
              None,
              Vector.empty,
              None
            ),
            ScalarTypeDefinition(
              "CustomScalar",
              Vector.empty,
              None,
              Vector.empty,
              None
            ),
            ScalarTypeDefinition(
              "AnnotatedScalar",
              Vector(
                Directive(
                  "onScalar",
                  Vector.empty,
                  Vector.empty,
                  None
                )),
              None,
              Vector.empty,
              None
            ),
            EnumTypeDefinition(
              "Site",
              Vector(
                EnumValueDefinition(
                  "DESKTOP",
                  Vector.empty,
                  None,
                  Vector(Comment(" value 1", None)),
                  None),
                EnumValueDefinition(
                  "MOBILE",
                  Vector.empty,
                  None,
                  Vector(Comment(" value 2", None)),
                  None)
              ),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              None
            ),
            EnumTypeDefinition(
              "AnnotatedEnum",
              Vector(
                EnumValueDefinition(
                  "ANNOTATED_VALUE",
                  Vector(
                    Directive(
                      "onEnumValue",
                      Vector.empty,
                      Vector.empty,
                      None
                    )),
                  None,
                  Vector.empty,
                  None),
                EnumValueDefinition("OTHER_VALUE", Vector.empty, None, Vector.empty, None)
              ),
              Vector(
                Directive(
                  "onEnum",
                  Vector.empty,
                  Vector.empty,
                  None
                )),
              None,
              Vector.empty,
              Vector.empty,
              None
            ),
            InputObjectTypeDefinition(
              "InputType",
              Vector(
                InputValueDefinition(
                  "key",
                  NotNullType(NamedType("String", None), None),
                  None,
                  Vector.empty,
                  None,
                  Vector.empty,
                  None),
                InputValueDefinition(
                  "answer",
                  NamedType("Int", None),
                  Some(BigIntValue(42, Vector.empty, None)),
                  Vector.empty,
                  None,
                  Vector.empty,
                  None)
              ),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              None
            ),
            InputObjectTypeDefinition(
              "AnnotatedInput",
              Vector(InputValueDefinition(
                "annotatedField",
                NamedType("Type", None),
                None,
                Vector(
                  Directive(
                    "onField",
                    Vector.empty,
                    Vector.empty,
                    None
                  )),
                None,
                Vector(Comment(" field comment", None)),
                None
              )),
              Vector(
                Directive(
                  "onInputObjectType",
                  Vector.empty,
                  Vector.empty,
                  None
                )),
              None,
              Vector.empty,
              Vector.empty,
              None
            ),
            ObjectTypeExtensionDefinition(
              "Foo",
              Vector.empty,
              Vector(FieldDefinition(
                "seven",
                NamedType("Type", None),
                Vector(
                  InputValueDefinition(
                    "argument",
                    ListType(NamedType("String", None), None),
                    None,
                    Vector.empty,
                    None,
                    Vector.empty,
                    None)),
                Vector.empty,
                None,
                Vector.empty,
                None
              )),
              Vector.empty,
              Vector.empty,
              Vector.empty,
              None
            ),
            ObjectTypeExtensionDefinition(
              "Foo",
              Vector.empty,
              Vector(
                FieldDefinition(
                  "foo",
                  NamedType("String", None),
                  Vector.empty,
                  Vector.empty,
                  None,
                  Vector.empty,
                  None)),
              Vector(
                Directive(
                  "onType",
                  Vector.empty,
                  Vector.empty,
                  None
                )),
              Vector.empty,
              Vector.empty,
              None
            ),
            DirectiveDefinition(
              "skip",
              Vector(
                InputValueDefinition(
                  "if",
                  NotNullType(NamedType("Boolean", None), None),
                  None,
                  Vector.empty,
                  None,
                  Vector.empty,
                  None)),
              Vector(
                DirectiveLocation("FIELD", Vector.empty, None),
                DirectiveLocation("FRAGMENT_SPREAD", Vector.empty, None),
                DirectiveLocation("INLINE_FRAGMENT", Vector.empty, None)
              ),
              None,
              false,
              Vector.empty,
              None
            ),
            DirectiveDefinition(
              "include",
              Vector(
                InputValueDefinition(
                  "if",
                  NotNullType(NamedType("Boolean", None), None),
                  None,
                  Vector.empty,
                  None,
                  Vector.empty,
                  None)),
              Vector(
                DirectiveLocation("FIELD", Vector.empty, None),
                DirectiveLocation("FRAGMENT_SPREAD", Vector.empty, None),
                DirectiveLocation("INLINE_FRAGMENT", Vector.empty, None)
              ),
              None,
              false,
              Vector.empty,
              None
            )
          ),
          Vector.empty,
          None,
          None
        ))
    }

    "parse input values with macro independently" in {
      val ast = graphqlInput"""
        {
          a: null

          # test comment
          b: 1
          c: {
            someNull: null
            enum: HELLO
          }
        }
      """

      ast should be(
        ObjectValue(
          Vector(
            ObjectField(
              "a",
              NullValue(Vector.empty, Some(AstLocation(24, 3, 14))),
              Vector.empty,
              Some(AstLocation(21, 3, 11))
            ),
            ObjectField(
              "b",
              BigIntValue(1, Vector.empty, Some(AstLocation(68, 6, 14))),
              Vector(Comment(" test comment", Some(AstLocation(40, 5, 11)))),
              Some(AstLocation(65, 6, 11))
            ),
            ObjectField(
              "c",
              ObjectValue(
                Vector(
                  ObjectField(
                    "someNull",
                    NullValue(Vector.empty, Some(AstLocation(107, 8, 23))),
                    Vector.empty,
                    Some(AstLocation(97, 8, 13))
                  ),
                  ObjectField(
                    "enum",
                    EnumValue("HELLO", Vector.empty, Some(AstLocation(130, 9, 19))),
                    Vector.empty,
                    Some(AstLocation(124, 9, 13))
                  )
                ),
                Vector.empty,
                Some(AstLocation(83, 7, 14))
              ),
              Vector.empty,
              Some(AstLocation(80, 7, 11))
            )
          ),
          Vector.empty,
          Some(AstLocation(9, 2, 9))
        ))
    }

    "fail compilation on syntax error in input values" in {
      """
        val ast = graphqlInput" {a: 1 c} "
      """ shouldNot compile
    }
  }
}
