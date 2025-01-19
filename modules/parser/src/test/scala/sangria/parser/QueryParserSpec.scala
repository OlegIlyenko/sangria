package sangria.parser

import language.postfixOps
import sangria.ast.AstLocation
import sangria.ast
import sangria.ast._
import sangria.util.{FileUtil, StringMatchers}

import scala.reflect.ClassTag
import scala.util.{Success, Try}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class QueryParserSpec extends AnyWordSpec with Matchers with StringMatchers {
  private[this] def parseQuery(query: String): Try[ast.Document] =
    QueryParser.parse(query, ParserConfig.default.withEmptySourceId.withoutSourceMapper)

  "QueryParser" should {
    "parse complex query" in {
      val query = FileUtil.loadQuery("complex-query.graphql")

      val expectedAst =
        Document(
          Vector(
            OperationDefinition(
              OperationType.Query,
              Some("FetchLukeAndLeiaAliased"),
              Vector(
                VariableDefinition(
                  "someVar",
                  NamedType("Int", Some(AstLocation(53, 2, 41))),
                  Some(BigDecimalValue(1.23, Vector.empty, Some(AstLocation(59, 2, 47)))),
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(43, 2, 31))
                ),
                VariableDefinition(
                  "anotherVar",
                  NamedType("Int", Some(AstLocation(77, 2, 65))),
                  Some(BigIntValue(123, Vector.empty, Some(AstLocation(83, 2, 71)))),
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(64, 2, 52))
                )
              ),
              Vector(
                Directive(
                  "include",
                  Vector(
                    Argument(
                      "if",
                      BooleanValue(true, Vector.empty, Some(AstLocation(100, 2, 88))),
                      Vector.empty,
                      Some(AstLocation(96, 2, 84))
                    )),
                  Vector.empty,
                  Some(AstLocation(87, 2, 75))
                ),
                Directive(
                  "include",
                  Vector(
                    Argument(
                      "if",
                      BooleanValue(false, Vector.empty, Some(AstLocation(119, 2, 107))),
                      Vector.empty,
                      Some(AstLocation(115, 2, 103))
                    )),
                  Vector.empty,
                  Some(AstLocation(106, 2, 94))
                )
              ),
              Vector(
                Field(
                  Some("luke"),
                  "human",
                  Vector(
                    Argument(
                      "id",
                      StringValue("1000", false, None, Vector.empty, Some(AstLocation(145, 3, 19))),
                      Vector.empty,
                      Some(AstLocation(141, 3, 15))
                    )),
                  Vector(Directive(
                    "include",
                    Vector(
                      Argument(
                        "if",
                        BooleanValue(true, Vector.empty, Some(AstLocation(165, 3, 39))),
                        Vector.empty,
                        Some(AstLocation(161, 3, 35))
                      )),
                    Vector.empty,
                    Some(AstLocation(152, 3, 26))
                  )),
                  Vector(Field(
                    None,
                    "friends",
                    Vector(
                      Argument(
                        "sort",
                        EnumValue("NAME", Vector.empty, Some(AstLocation(190, 4, 19))),
                        Vector.empty,
                        Some(AstLocation(184, 4, 13))
                      )),
                    Vector.empty,
                    Vector.empty,
                    Vector.empty,
                    Vector.empty,
                    Some(AstLocation(176, 4, 5))
                  )),
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(129, 3, 3))
                ),
                Field(
                  Some("leia"),
                  "human",
                  Vector(
                    Argument(
                      "id",
                      StringValue(
                        "10103\n \u00F6 \u00F6",
                        false,
                        None,
                        Vector.empty,
                        Some(AstLocation(223, 6, 24))),
                      Vector.empty,
                      Some(AstLocation(214, 6, 15))
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
                      Some(AstLocation(249, 7, 5))
                    )),
                  Vector.empty,
                  Vector(Comment(" some name", Some(AstLocation(254, 7, 10)))),
                  Some(AstLocation(202, 6, 3))
                ),
                InlineFragment(
                  Some(NamedType("User", Some(AstLocation(280, 10, 10)))),
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
                        Some(AstLocation(297, 11, 11))
                      )),
                    Vector.empty,
                    Vector.empty,
                    Some(AstLocation(291, 11, 5))
                  )),
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(273, 10, 3))
                ),
                FragmentSpread("Foo", Vector.empty, Vector.empty, Some(AstLocation(309, 14, 3)))
              ),
              Vector(Comment(" test query", Some(AstLocation(0, 1, 1)))),
              Vector.empty,
              Some(AstLocation(13, 2, 1))
            ),
            FragmentDefinition(
              "Foo",
              NamedType("User", Some(AstLocation(335, 17, 17))),
              Vector(
                Directive(
                  "foo",
                  Vector(
                    Argument(
                      "bar",
                      BigIntValue(1, Vector.empty, Some(AstLocation(350, 17, 32))),
                      Vector.empty,
                      Some(AstLocation(345, 17, 27))
                    )),
                  Vector.empty,
                  Some(AstLocation(340, 17, 22))
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
                  Some(AstLocation(356, 18, 3))
                )),
              Vector.empty,
              Vector.empty,
              Vector(Comment(" field in fragment!", Some(AstLocation(360, 18, 7)))),
              Some(AstLocation(319, 17, 1))
            )
          ),
          Vector.empty,
          Some(AstLocation(0, 1, 1)),
          None
        )

      parseQuery(query) should be(Success(expectedAst))
    }

    "parse kitchen sink" in {
      val query = FileUtil.loadQuery("kitchen-sink.graphql")

      val expectedAst =
        Document(
          Vector(
            OperationDefinition(
              OperationType.Query,
              Some("queryName"),
              Vector(
                VariableDefinition(
                  "foo",
                  NamedType("ComplexType", Some(AstLocation(310, 8, 23))),
                  None,
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(304, 8, 17))
                ),
                VariableDefinition(
                  "site",
                  NamedType("Site", Some(AstLocation(330, 8, 43))),
                  Some(EnumValue("MOBILE", Vector.empty, Some(AstLocation(337, 8, 50)))),
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(323, 8, 36))
                )
              ),
              Vector.empty,
              Vector(
                Field(
                  Some("whoever123is"),
                  "node",
                  Vector(Argument(
                    "id",
                    ListValue(
                      Vector(
                        BigIntValue(123, Vector.empty, Some(AstLocation(373, 9, 27))),
                        BigIntValue(456, Vector.empty, Some(AstLocation(378, 9, 32)))),
                      Vector.empty,
                      Some(AstLocation(372, 9, 26))
                    ),
                    Vector.empty,
                    Some(AstLocation(368, 9, 22))
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
                      Some(AstLocation(390, 10, 5))
                    ),
                    InlineFragment(
                      Some(NamedType("User", Some(AstLocation(406, 11, 12)))),
                      Vector(
                        Directive(
                          "defer",
                          Vector.empty,
                          Vector.empty,
                          Some(AstLocation(411, 11, 17))
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
                              Some(AstLocation(443, 13, 9))
                            ),
                            Field(
                              Some("alias"),
                              "field1",
                              Vector(
                                Argument(
                                  "first",
                                  BigIntValue(10, Vector.empty, Some(AstLocation(476, 14, 29))),
                                  Vector.empty,
                                  Some(AstLocation(470, 14, 23))
                                ),
                                Argument(
                                  "after",
                                  VariableValue(
                                    "foo",
                                    Vector.empty,
                                    Some(AstLocation(486, 14, 39))),
                                  Vector.empty,
                                  Some(AstLocation(480, 14, 33))
                                )
                              ),
                              Vector(Directive(
                                "include",
                                Vector(
                                  Argument(
                                    "if",
                                    VariableValue(
                                      "foo",
                                      Vector.empty,
                                      Some(AstLocation(506, 14, 59))),
                                    Vector.empty,
                                    Some(AstLocation(502, 14, 55))
                                  )),
                                Vector.empty,
                                Some(AstLocation(493, 14, 46))
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
                                  Some(AstLocation(524, 15, 11))
                                ),
                                FragmentSpread(
                                  "frag",
                                  Vector.empty,
                                  Vector.empty,
                                  Some(AstLocation(538, 16, 11)))
                              ),
                              Vector.empty,
                              Vector.empty,
                              Some(AstLocation(456, 14, 9))
                            )
                          ),
                          Vector.empty,
                          Vector.empty,
                          Some(AstLocation(426, 12, 7))
                        )),
                      Vector.empty,
                      Vector.empty,
                      Some(AstLocation(399, 11, 5))
                    )
                  ),
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(349, 9, 3))
                )),
              Vector(
                Comment(" Copyright (c) 2015, Facebook, Inc.", Some(AstLocation(0, 1, 1))),
                Comment(" All rights reserved.", Some(AstLocation(37, 2, 1))),
                Comment("", Some(AstLocation(60, 3, 1))),
                Comment(
                  " This source code is licensed under the BSD-style license found in the",
                  Some(AstLocation(62, 4, 1))),
                Comment(
                  " LICENSE file in the root directory of this source tree. An additional grant",
                  Some(AstLocation(134, 5, 1))),
                Comment(
                  " of patent rights can be found in the PATENTS file in the same directory.",
                  Some(AstLocation(212, 6, 1)))
              ),
              Vector.empty,
              Some(AstLocation(288, 8, 1))
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
                      BigIntValue(123, Vector.empty, Some(AstLocation(612, 24, 15))),
                      Vector.empty,
                      Some(AstLocation(605, 24, 8))
                    )),
                  Vector(
                    Directive(
                      "defer",
                      Vector.empty,
                      Vector.empty,
                      Some(AstLocation(617, 24, 20))
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
                        Some(AstLocation(644, 26, 7))
                      )),
                    Vector.empty,
                    Vector.empty,
                    Some(AstLocation(630, 25, 5))
                  )),
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(600, 24, 3))
                )),
              Vector.empty,
              Vector.empty,
              Some(AstLocation(577, 23, 1))
            ),
            OperationDefinition(
              OperationType.Subscription,
              Some("StoryLikeSubscription"),
              Vector(
                VariableDefinition(
                  "input",
                  NamedType("StoryLikeSubscribeInput", Some(AstLocation(703, 31, 44))),
                  None,
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(695, 31, 36))
                )),
              Vector.empty,
              Vector(
                Field(
                  None,
                  "storyLikeSubscribe",
                  Vector(
                    Argument(
                      "input",
                      VariableValue("input", Vector.empty, Some(AstLocation(758, 32, 29))),
                      Vector.empty,
                      Some(AstLocation(751, 32, 22))
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
                              Some(AstLocation(803, 35, 9))
                            )),
                          Vector.empty,
                          Vector.empty,
                          Some(AstLocation(786, 34, 7))
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
                              Some(AstLocation(846, 38, 9))
                            )),
                          Vector.empty,
                          Vector.empty,
                          Some(AstLocation(823, 37, 7))
                        )
                      ),
                      Vector.empty,
                      Vector.empty,
                      Some(AstLocation(772, 33, 5))
                    )),
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(732, 32, 3))
                )),
              Vector.empty,
              Vector.empty,
              Some(AstLocation(660, 31, 1))
            ),
            FragmentDefinition(
              "frag",
              NamedType("Friend", Some(AstLocation(889, 44, 18))),
              Vector.empty,
              Vector(
                Field(
                  None,
                  "foo",
                  Vector(
                    Argument(
                      "size",
                      VariableValue("size", Vector.empty, Some(AstLocation(910, 45, 13))),
                      Vector.empty,
                      Some(AstLocation(904, 45, 7))
                    ),
                    Argument(
                      "bar",
                      VariableValue("b", Vector.empty, Some(AstLocation(922, 45, 25))),
                      Vector.empty,
                      Some(AstLocation(917, 45, 20))
                    ),
                    Argument(
                      "obj",
                      ObjectValue(
                        Vector(
                          ObjectField(
                            "key",
                            StringValue(
                              "value",
                              false,
                              None,
                              Vector.empty,
                              Some(AstLocation(937, 45, 40))),
                            Vector.empty,
                            Some(AstLocation(932, 45, 35))
                          )),
                        Vector.empty,
                        Some(AstLocation(931, 45, 34))
                      ),
                      Vector.empty,
                      Some(AstLocation(926, 45, 29))
                    )
                  ),
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(900, 45, 3))
                )),
              Vector.empty,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(872, 44, 1))
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
                      BooleanValue(true, Vector.empty, Some(AstLocation(970, 49, 19))),
                      Vector.empty,
                      Some(AstLocation(962, 49, 11))
                    ),
                    Argument(
                      "falsey",
                      BooleanValue(false, Vector.empty, Some(AstLocation(984, 49, 33))),
                      Vector.empty,
                      Some(AstLocation(976, 49, 25))
                    )
                  ),
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(954, 49, 3))
                ),
                Field(
                  None,
                  "query",
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(994, 50, 3))
                ),
                InlineFragment(
                  None,
                  Vector(Directive(
                    "skip",
                    Vector(
                      Argument(
                        "unless",
                        VariableValue("foo", Vector.empty, Some(AstLocation(1021, 52, 21))),
                        Vector.empty,
                        Some(AstLocation(1013, 52, 13))
                      )),
                    Vector.empty,
                    Some(AstLocation(1007, 52, 7))
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
                      Some(AstLocation(1033, 53, 5))
                    )),
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(1003, 52, 3))
                ),
                InlineFragment(
                  None,
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
                      Some(AstLocation(1052, 56, 5))
                    )),
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(1042, 55, 3))
                )
              ),
              Vector.empty,
              Vector.empty,
              Some(AstLocation(950, 48, 1))
            )
          ),
          Vector.empty,
          Some(AstLocation(0, 1, 1)),
          None
        )

      parseQuery(query) should be(Success(expectedAst))
    }

    "parse kitchen sink without comments and locations" in {
      val config =
        ParserConfig.default.withoutLocations.withoutComments.withEmptySourceId.withoutSourceMapper

      val query = FileUtil.loadQuery("kitchen-sink.graphql")

      val expectedAst =
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
                ),
                InlineFragment(
                  None,
                  Vector(
                    Directive(
                      "skip",
                      Vector(
                        Argument(
                          "unless",
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
                    )),
                  Vector.empty,
                  Vector.empty,
                  None
                ),
                InlineFragment(
                  None,
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

      QueryParser.parse(query, config) should be(Success(expectedAst))
    }

    "parse anonymous query" in {
      val query =
        """
          query {
            foo bar,
            baz
          }
        """

      val expectedAst =
        Document(
          Vector(
            OperationDefinition(
              OperationType.Query,
              None,
              Vector.empty,
              Vector.empty,
              Vector(
                Field(
                  None,
                  "foo",
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(31, 3, 13))),
                Field(
                  None,
                  "bar",
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(35, 3, 17))),
                Field(
                  None,
                  "baz",
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(52, 4, 13)))
              ),
              Vector.empty,
              Vector.empty,
              Some(AstLocation(11, 2, 11))
            )),
          Vector.empty,
          Some(AstLocation(11, 2, 11)),
          None
        )

      parseQuery(stripCarriageReturns(query)) should be(Success(expectedAst))
    }

    "parse inline fragments without type condition" in {
      val query =
        """
          query {
            ... {
              foo bar
            }

            ... @include(if: true) {
              baz
            }
          }
        """

      val expectedAst =
        Document(
          Vector(
            OperationDefinition(
              OperationType.Query,
              None,
              Vector.empty,
              Vector.empty,
              Vector(
                InlineFragment(
                  None,
                  Vector.empty,
                  Vector(
                    Field(
                      None,
                      "foo",
                      Vector.empty,
                      Vector.empty,
                      Vector.empty,
                      Vector.empty,
                      Vector.empty,
                      Some(AstLocation(51, 4, 15))),
                    Field(
                      None,
                      "bar",
                      Vector.empty,
                      Vector.empty,
                      Vector.empty,
                      Vector.empty,
                      Vector.empty,
                      Some(AstLocation(55, 4, 19)))
                  ),
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(31, 3, 13))
                ),
                InlineFragment(
                  None,
                  Vector(Directive(
                    "include",
                    Vector(
                      Argument(
                        "if",
                        BooleanValue(true, Vector.empty, Some(AstLocation(103, 7, 30))),
                        Vector.empty,
                        Some(AstLocation(99, 7, 26)))),
                    Vector.empty,
                    Some(AstLocation(90, 7, 17))
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
                      Some(AstLocation(125, 8, 15)))),
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(86, 7, 13))
                )
              ),
              Vector.empty,
              Vector.empty,
              Some(AstLocation(11, 2, 11))
            )),
          Vector.empty,
          Some(AstLocation(11, 2, 11)),
          None
        )

      parseQuery(stripCarriageReturns(query)) should be(Success(expectedAst))
    }

    "parse anonymous mutation" in {
      val query =
        """
          mutation {
            foo bar,
            baz
          }
        """

      val expectedAst =
        Document(
          Vector(
            OperationDefinition(
              OperationType.Mutation,
              None,
              Vector.empty,
              Vector.empty,
              Vector(
                Field(
                  None,
                  "foo",
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(34, 3, 13))),
                Field(
                  None,
                  "bar",
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(38, 3, 17))),
                Field(
                  None,
                  "baz",
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(55, 4, 13)))
              ),
              Vector.empty,
              Vector.empty,
              Some(AstLocation(11, 2, 11))
            )),
          Vector.empty,
          Some(AstLocation(11, 2, 11)),
          None
        )

      parseQuery(stripCarriageReturns(query)) should be(Success(expectedAst))
    }

    "provide useful error message (fragment `on`)" in {
      val error = parseQuery("""
          { ...MissingOn }
          fragment MissingOn Type
        """).failed.get.asInstanceOf[SyntaxError]

      error.formattedError should equal(
        """Invalid input 'T', expected ExperimentalFragmentVariables or TypeCondition (line 3, column 30):
          |          fragment MissingOn Type
          |                             ^""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "provide useful error message (braces)" in {
      val error = parseQuery("{ field: {} }").failed.get.asInstanceOf[SyntaxError]

      error.formattedError should equal(
        """Invalid input "{ field: {", expected ExecutableDefinition or TypeSystemDefinition (line 1, column 1):
          |{ field: {} }
          |^""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "provide useful error message (operation def)" in {
      val error = parseQuery("notanoperation Foo { field }").failed.get.asInstanceOf[SyntaxError]

      error.formattedError should equal(
        """Invalid input 'n', expected ExecutableDefinition or TypeSystemDefinition (line 1, column 1):
          |notanoperation Foo { field }
          |^""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "provide useful error message (ellipsis)" in {
      val error = parseQuery("...").failed.get.asInstanceOf[SyntaxError]

      error.formattedError should equal(
        """Invalid input '.', expected ExecutableDefinition or TypeSystemDefinition (line 1, column 1):
          |...
          |^""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "parses constant default values" in {
      parseQuery("{ field(complex: { a: { b: [ $var ] } }) }").isSuccess should be(true)
    }

    "parses variable definition directives" in {
      parseQuery("query Foo($x: Boolean = false @bar) { field }").isSuccess should be(true)
    }

    "parses variable inline values" in {
      val error =
        parseQuery("query Foo($x: Complex = { a: { b: [ $var ] } }) { field }").failed.get
          .asInstanceOf[SyntaxError]

      error.getMessage should equal(
        """Syntax error while parsing GraphQL query. Invalid input '$', expected NumberValue, StringValue, BooleanValue, NullValue, EnumValue, ListValueConst or ObjectValueConst (line 1, column 37):
          |query Foo($x: Complex = { a: { b: [ $var ] } }) { field }
          |                                    ^""".stripMargin)(
        after.being(strippedOfCarriageReturns))
    }

    "produce parse error for `1.`" in {
      val error =
        parseQuery("query Foo($x: Complex = 1.) { field }").failed.get.asInstanceOf[SyntaxError]

      error.formattedError should equal(
        """Invalid input "1.)", expected ValueConst, DirectivesConst or VariableDefinition (line 1, column 25):
          |query Foo($x: Complex = 1.) { field }
          |                        ^""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "produce parse error for `.123`" in {
      val error =
        parseQuery("query Foo($x: Complex = .123) { field }").failed.get.asInstanceOf[SyntaxError]

      error.formattedError should equal(
        """Invalid input '.', expected NumberValue, StringValue, BooleanValue, NullValue, EnumValue, ListValueConst or ObjectValueConst (line 1, column 25):
          |query Foo($x: Complex = .123) { field }
          |                        ^""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "produce parse error for `1.0e`" in {
      val error =
        parseQuery("query Foo($x: Complex = 1.0e) { field }").failed.get.asInstanceOf[SyntaxError]

      error.formattedError should equal(
        """Invalid input "1.0e)", expected ValueConst, DirectivesConst or VariableDefinition (line 1, column 25):
          |query Foo($x: Complex = 1.0e) { field }
          |                        ^""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "produce parse error for `1.A`" in {
      val error =
        parseQuery("query Foo($x: Complex = 1.A) { field }").failed.get.asInstanceOf[SyntaxError]

      error.formattedError should equal(
        """Invalid input "1.A", expected ValueConst, DirectivesConst or VariableDefinition (line 1, column 25):
          |query Foo($x: Complex = 1.A) { field }
          |                        ^""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "produce parse error for `+1`" in {
      val error =
        parseQuery("query Foo($x: Complex = +1) { field }").failed.get.asInstanceOf[SyntaxError]

      error.formattedError should equal(
        """Invalid input '+', expected NumberValue, StringValue, BooleanValue, NullValue, EnumValue, ListValueConst or ObjectValueConst (line 1, column 25):
          |query Foo($x: Complex = +1) { field }
          |                        ^""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "produce parse error for `1.0eA`" in {
      val error =
        parseQuery("query Foo($x: Complex = 1.0eA) { field }").failed.get.asInstanceOf[SyntaxError]

      error.formattedError should equal(
        """Invalid input "1.0eA", expected ValueConst, DirectivesConst or VariableDefinition (line 1, column 25):
          |query Foo($x: Complex = 1.0eA) { field }
          |                        ^""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "disallows uncommon control characters" in {
      parseQuery("{ field\u0007 }").isSuccess should be(false)
      parseQuery("{ field } \u0007").isSuccess should be(false)
    }

    "accepts BOM header" in {
      parseQuery("\uFEFF{ field }").isSuccess should be(true)
    }

    "accepts new lines header" in {
      parseQuery("{ field \n another }").isSuccess should be(true)
      parseQuery("{ field \r\n another }").isSuccess should be(true)
    }

    "accepts escape sequences" in {
      parseQuery("{ field(id: \"\\u000A\") }").isSuccess should be(true)
      parseQuery("{ field(id: \"\\uXXXX\") }").isSuccess should be(false)
      parseQuery("{ field(id: \"\\x\") }").isSuccess should be(false)
    }

    "allow `null` to be the prefix of an enum value" in {
      parseQuery("query Foo($x: Complex = null111) { field }").isSuccess should be(true)
      parseQuery("query Foo($x: Complex = null_foo) { field }").isSuccess should be(true)
      parseQuery("query Foo($x: Complex = nullFoo) { field }").isSuccess should be(true)
    }

    "parse leading vertical bar in union types" in {
      val ast = parseQuery("union Hello = | Wo | Rld").get

      ast.withoutSourceMapper should be(
        Document(
          Vector(UnionTypeDefinition(
            "Hello",
            Vector(
              NamedType("Wo", Some(AstLocation(16, 1, 17))),
              NamedType("Rld", Some(AstLocation(21, 1, 22)))),
            Vector.empty,
            None,
            Vector.empty,
            Some(AstLocation(0, 1, 1))
          )),
          Vector.empty,
          Some(AstLocation(0, 1, 1)),
          None
        ))
    }

    "not parse invalid usage of vertical bar on union types" in {
      parseQuery("union Hello = |").isSuccess should be(false)
      parseQuery("union Hello = Wo | Rld |").isSuccess should be(false)
      parseQuery("union Hello = || Wo | Rld").isSuccess should be(false)
      parseQuery("union Hello = Wo || Rld").isSuccess should be(false)
      parseQuery("union Hello = | Wo | Rld ||").isSuccess should be(false)
    }

    "parse leading vertical bar in directive definitions" in {
      val ast = parseQuery("""
        directive @include2(if: Boolean!) on
          | FIELD
          | FRAGMENT_SPREAD
          | INLINE_FRAGMENT
        """.stripCR).get

      ast.withoutSourceMapper should be(
        Document(
          Vector(
            DirectiveDefinition(
              "include2",
              Vector(InputValueDefinition(
                "if",
                NotNullType(
                  NamedType("Boolean", Some(AstLocation(33, 2, 33))),
                  Some(AstLocation(33, 2, 33))),
                None,
                Vector.empty,
                None,
                Vector.empty,
                Some(AstLocation(29, 2, 29))
              )),
              Vector(
                DirectiveLocation("FIELD", Vector.empty, Some(AstLocation(58, 3, 13))),
                DirectiveLocation("FRAGMENT_SPREAD", Vector.empty, Some(AstLocation(76, 4, 13))),
                DirectiveLocation("INLINE_FRAGMENT", Vector.empty, Some(AstLocation(104, 5, 13)))
              ),
              None,
              false,
              Vector.empty,
              Some(AstLocation(9, 2, 9))
            )),
          Vector.empty,
          Some(AstLocation(9, 2, 9)),
          None
        ))
    }

    def findAst[T <: AstNode: ClassTag](ast: AstNode): Option[T] =
      ast match {
        case node if implicitly[ClassTag[T]].runtimeClass.isAssignableFrom(node.getClass) =>
          Some(node.asInstanceOf[T])
        case Document(defs, _, _, _) => defs.map(findAst[T]).find(_.isDefined).flatten
        case OperationDefinition(_, _, vars, _, _, _, _, _) =>
          vars.map(findAst[T]).find(_.isDefined).flatten
        case VariableDefinition(_, _, default, _, _, _) => default.flatMap(findAst[T])
        case _ => None
      }

    "parse int values" in {
      val expectedTable = Vector(
        "4" -> BigInt("4"),
        "-4" -> BigInt("-4"),
        "9" -> BigInt("9"),
        "0" -> BigInt("0"),
        "784236564875237645762347623147574756321" -> BigInt(
          "784236564875237645762347623147574756321")
      )

      expectedTable.foreach { expected =>
        findAst[BigIntValue](
          parseQuery(s"query Foo($$x: Complex = ${expected._1}) { field }").get) should be(
          Some(BigIntValue(expected._2, Vector.empty, Some(AstLocation(24, 1, 25)))))
      }
    }

    "parse float values" in {
      val expectedTable = Vector(
        "4.123" -> BigDecimal("4.123"),
        "-4.123" -> BigDecimal("-4.123"),
        "0.123" -> BigDecimal("0.123"),
        "123E4" -> BigDecimal("123E4"),
        "123e-4" -> BigDecimal("123e-4"),
        "-1.123e4" -> BigDecimal("-1.123e4"),
        "-1.123E4" -> BigDecimal("-1.123E4"),
        "-1.123e+4" -> BigDecimal("-1.123e+4"),
        "-1.123e4567" -> BigDecimal("-1.123e4567")
      )

      expectedTable.foreach { expected =>
        withClue(s"Parsing ${expected._1}.") {
          findAst[BigDecimalValue](
            parseQuery(s"query Foo($$x: Complex = ${expected._1}) { field }").get) should be(
            Some(BigDecimalValue(expected._2, Vector.empty, Some(AstLocation(24, 1, 25)))))
        }
      }
    }

    "parse block string values" in {
      val q = "\"\"\""

      val stringValue =
        s"""
          $q
            hello,
              world
          $q
        """

      QueryParser.parseInput(stripCarriageReturns(stringValue)) should be(
        Success(
          StringValue(
            "hello,\n  world",
            true,
            Some("\n            hello,\n              world\n          "),
            Vector.empty,
            Some(AstLocation(11, 2, 11)))))
    }

    "parse input values independently" in {
      val expectedTable = Vector(
        "null" -> NullValue(Vector.empty, Some(AstLocation(0, 1, 1))),
        "1.234" -> BigDecimalValue(BigDecimal("1.234"), Vector.empty, Some(AstLocation(0, 1, 1))),
        "HELLO_WORLD" -> EnumValue("HELLO_WORLD", Vector.empty, Some(AstLocation(0, 1, 1))),
        "[1, 2 \"test\"]" -> ListValue(
          Vector(
            BigIntValue(1, Vector.empty, Some(AstLocation(1, 1, 2))),
            BigIntValue(2, Vector.empty, Some(AstLocation(4, 1, 5))),
            StringValue("test", false, None, Vector.empty, Some(AstLocation(6, 1, 7)))
          ),
          Vector.empty,
          Some(AstLocation(0, 1, 1))
        ),
        "{a: 1, b: \"foo\" c: {nest: true, oops: null, e: FOO_BAR}}" ->
          ObjectValue(
            Vector(
              ObjectField(
                "a",
                BigIntValue(1, Vector.empty, Some(AstLocation(4, 1, 5))),
                Vector.empty,
                Some(AstLocation(1, 1, 2))),
              ObjectField(
                "b",
                StringValue("foo", false, None, Vector.empty, Some(AstLocation(10, 1, 11))),
                Vector.empty,
                Some(AstLocation(7, 1, 8))),
              ObjectField(
                "c",
                ObjectValue(
                  Vector(
                    ObjectField(
                      "nest",
                      BooleanValue(true, Vector.empty, Some(AstLocation(26, 1, 27))),
                      Vector.empty,
                      Some(AstLocation(20, 1, 21))),
                    ObjectField(
                      "oops",
                      NullValue(Vector.empty, Some(AstLocation(38, 1, 39))),
                      Vector.empty,
                      Some(AstLocation(32, 1, 33))),
                    ObjectField(
                      "e",
                      EnumValue("FOO_BAR", Vector.empty, Some(AstLocation(47, 1, 48))),
                      Vector.empty,
                      Some(AstLocation(44, 1, 45)))
                  ),
                  Vector.empty,
                  Some(AstLocation(19, 1, 20))
                ),
                Vector.empty,
                Some(AstLocation(16, 1, 17))
              )
            ),
            Vector.empty,
            Some(AstLocation(0, 1, 1))
          ),
        """
         {
           a: 1

           # This is a test comment!
           b: "foo"
         }
        """ ->
          ObjectValue(
            Vector(
              ObjectField(
                "a",
                BigIntValue(1, Vector.empty, Some(AstLocation(26, 3, 15))),
                Vector.empty,
                Some(AstLocation(23, 3, 12))),
              ObjectField(
                "b",
                StringValue("foo", false, None, Vector.empty, Some(AstLocation(80, 6, 15))),
                Vector(Comment(" This is a test comment!", Some(AstLocation(40, 5, 12)))),
                Some(AstLocation(77, 6, 12))
              )
            ),
            Vector.empty,
            Some(AstLocation(10, 2, 10))
          )
      )

      expectedTable.foreach { expected =>
        withClue(s"Parsing ${expected._1}.") {
          QueryParser.parseInput(stripCarriageReturns(expected._1)) should equal(
            Success(expected._2))
        }
      }
    }

    "parse and collect comments in AST nodes" in {
      val query = FileUtil.loadQuery("too-many-comments.graphql")

      val expected =
        Document(
          Vector(
            OperationDefinition(
              OperationType.Query,
              Some("queryName"),
              Vector(
                VariableDefinition(
                  "foo",
                  NamedType("ComplexType", Some(AstLocation(434, 23, 1))),
                  None,
                  Vector.empty,
                  Vector(
                    Comment(" comment 5", Some(AstLocation(354, 15, 1))),
                    Comment(" comment 6", Some(AstLocation(366, 16, 1)))),
                  Some(AstLocation(378, 17, 1))
                ),
                VariableDefinition(
                  "site",
                  NamedType("Site", Some(AstLocation(565, 36, 1))),
                  Some(EnumValue(
                    "MOBILE",
                    Vector(
                      Comment(" comment 16.5", Some(AstLocation(602, 40, 1))),
                      Comment(" comment 16.6", Some(AstLocation(617, 41, 1)))),
                    Some(AstLocation(632, 42, 1))
                  )),
                  Vector.empty,
                  Vector(
                    Comment(" comment 11", Some(AstLocation(446, 24, 1))),
                    Comment(" comment 12", Some(AstLocation(459, 25, 1))),
                    Comment(" comment 13", Some(AstLocation(475, 28, 1))),
                    Comment(" comment 14", Some(AstLocation(488, 29, 1)))
                  ),
                  Some(AstLocation(501, 30, 1))
                ),
                VariableDefinition(
                  "foo",
                  NamedType("ComplexType", Some(AstLocation(703, 48, 7))),
                  Some(
                    ObjectValue(
                      Vector(
                        ObjectField(
                          "field1",
                          StringValue(
                            "val",
                            false,
                            None,
                            Vector(
                              Comment(" comment 18.11", Some(AstLocation(849, 61, 1))),
                              Comment(" comment 18.12", Some(AstLocation(865, 62, 1)))),
                            Some(AstLocation(881, 63, 1))
                          ),
                          Vector(
                            Comment(" comment 18.7", Some(AstLocation(779, 55, 1))),
                            Comment(" comment 18.8", Some(AstLocation(794, 56, 1)))),
                          Some(AstLocation(809, 57, 1))
                        ),
                        ObjectField(
                          "list",
                          ListValue(
                            Vector(
                              BigIntValue(
                                1,
                                Vector(
                                  Comment(" comment 18.21", Some(AstLocation(1026, 76, 1))),
                                  Comment(" comment 18.22", Some(AstLocation(1042, 77, 1)))),
                                Some(AstLocation(1058, 78, 1))
                              ),
                              BigIntValue(
                                2,
                                Vector(
                                  Comment(" comment 18.23", Some(AstLocation(1061, 79, 1))),
                                  Comment(" comment 18.24", Some(AstLocation(1077, 80, 1)))),
                                Some(AstLocation(1093, 81, 1))
                              ),
                              BigIntValue(
                                3,
                                Vector(
                                  Comment(" comment 18.25", Some(AstLocation(1096, 82, 1))),
                                  Comment(" comment 18.26", Some(AstLocation(1112, 83, 1)))),
                                Some(AstLocation(1128, 84, 1))
                              )
                            ),
                            Vector(
                              Comment(" comment 18.19", Some(AstLocation(992, 73, 1))),
                              Comment(" comment 18.20", Some(AstLocation(1008, 74, 1)))),
                            Some(AstLocation(1024, 75, 1))
                          ),
                          Vector(
                            Comment(" comment 18.13", Some(AstLocation(887, 64, 1))),
                            Comment(" comment 18.14", Some(AstLocation(903, 65, 1))),
                            Comment(" comment 18.15", Some(AstLocation(921, 67, 1))),
                            Comment(" comment 18.16", Some(AstLocation(937, 68, 1)))
                          ),
                          Some(AstLocation(953, 69, 1))
                        ),
                        ObjectField(
                          "field2",
                          BooleanValue(
                            true,
                            Vector(
                              Comment(" comment 18.35", Some(AstLocation(1271, 97, 1))),
                              Comment(" comment 18.36", Some(AstLocation(1287, 98, 1)))),
                            Some(AstLocation(1303, 99, 1))
                          ),
                          Vector(
                            Comment(" comment 18.29", Some(AstLocation(1164, 88, 1))),
                            Comment(" comment 18.30", Some(AstLocation(1180, 89, 1))),
                            Comment(" comment 18.31", Some(AstLocation(1198, 91, 1))),
                            Comment(" comment 18.32", Some(AstLocation(1214, 92, 1)))
                          ),
                          Some(AstLocation(1230, 93, 1))
                        )
                      ),
                      Vector(
                        Comment(" comment 18.5", Some(AstLocation(747, 52, 1))),
                        Comment(" comment 18.6", Some(AstLocation(762, 53, 1)))),
                      Some(AstLocation(777, 54, 1))
                    )),
                  Vector.empty,
                  Vector(
                    Comment(" comment 17", Some(AstLocation(639, 43, 1))),
                    Comment(" comment 18", Some(AstLocation(652, 44, 1))),
                    Comment(" comment 18.1", Some(AstLocation(667, 46, 1))),
                    Comment(" comment 18.2", Some(AstLocation(682, 47, 1)))
                  ),
                  Some(AstLocation(697, 48, 1))
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
                          BigIntValue(
                            123,
                            Vector(
                              Comment(" comment 35", Some(AstLocation(1660, 130, 3))),
                              Comment(" comment 36", Some(AstLocation(1675, 131, 3)))),
                            Some(AstLocation(1690, 132, 3))),
                          BigIntValue(
                            456,
                            Vector(
                              Comment(" comment 37", Some(AstLocation(1696, 133, 3))),
                              Comment(" comment 38", Some(AstLocation(1711, 134, 3)))),
                            Some(AstLocation(1726, 135, 3)))
                        ),
                        Vector(
                          Comment(" comment 33", Some(AstLocation(1626, 127, 3))),
                          Comment(" comment 34", Some(AstLocation(1641, 128, 3)))),
                        Some(AstLocation(1656, 129, 3))
                      ),
                      Vector(
                        Comment(" comment 29", Some(AstLocation(1557, 121, 3))),
                        Comment(" comment 30", Some(AstLocation(1572, 122, 3)))),
                      Some(AstLocation(1587, 123, 3))
                    )),
                  Vector.empty,
                  Vector(
                    Field(
                      None,
                      "id",
                      Vector.empty,
                      Vector.empty,
                      Vector.empty,
                      Vector(
                        Comment(" comment 44", Some(AstLocation(1837, 145, 4))),
                        Comment(" comment 45", Some(AstLocation(1853, 146, 4)))),
                      Vector.empty,
                      Some(AstLocation(1870, 147, 5))
                    ),
                    InlineFragment(
                      Some(NamedType("User", Some(AstLocation(1996, 156, 5)))),
                      Vector(Directive(
                        "defer",
                        Vector.empty,
                        Vector(
                          Comment(" comment 52", Some(AstLocation(2005, 157, 5))),
                          Comment(" comment 53", Some(AstLocation(2022, 158, 5)))),
                        Some(AstLocation(2039, 159, 5))
                      )),
                      Vector(
                        Field(
                          None,
                          "field2",
                          Vector.empty,
                          Vector.empty,
                          Vector(
                            Field(
                              Some("alias"),
                              "field1",
                              Vector(
                                Argument(
                                  "first",
                                  BigIntValue(
                                    10,
                                    Vector(
                                      Comment(" comment 70", Some(AstLocation(2474, 185, 9))),
                                      Comment(" comment 71", Some(AstLocation(2495, 186, 9)))),
                                    Some(AstLocation(2516, 187, 9))),
                                  Vector(
                                    Comment(" comment 66", Some(AstLocation(2366, 179, 9))),
                                    Comment(" comment 67", Some(AstLocation(2387, 180, 9)))),
                                  Some(AstLocation(2408, 181, 9))
                                ),
                                Argument(
                                  "after",
                                  VariableValue(
                                    "foo",
                                    Vector(
                                      Comment(" comment 76", Some(AstLocation(2636, 194, 9))),
                                      Comment(" comment 77", Some(AstLocation(2657, 195, 9)))),
                                    Some(AstLocation(2678, 196, 9))
                                  ),
                                  Vector(
                                    Comment(" comment 72", Some(AstLocation(2528, 188, 9))),
                                    Comment(" comment 73", Some(AstLocation(2549, 189, 9)))),
                                  Some(AstLocation(2570, 190, 9))
                                )
                              ),
                              Vector(
                                Directive(
                                  "include",
                                  Vector(
                                    Argument(
                                      "if",
                                      VariableValue(
                                        "foo",
                                        Vector(
                                          Comment(" comment 88", Some(AstLocation(2961, 212, 10))),
                                          Comment(" comment 89", Some(AstLocation(2983, 213, 10)))),
                                        Some(AstLocation(3005, 214, 10))
                                      ),
                                      Vector(
                                        Comment(" comment 84", Some(AstLocation(2855, 206, 9))),
                                        Comment(" comment 85", Some(AstLocation(2876, 207, 9)))),
                                      Some(AstLocation(2897, 208, 9))
                                    )),
                                  Vector(
                                    Comment(" comment 80", Some(AstLocation(2744, 200, 9))),
                                    Comment(" comment 81", Some(AstLocation(2765, 201, 9)))),
                                  Some(AstLocation(2786, 202, 9))
                                )),
                              Vector(
                                Field(
                                  None,
                                  "id",
                                  Vector.empty,
                                  Vector.empty,
                                  Vector.empty,
                                  Vector(
                                    Comment(" comment 94", Some(AstLocation(3130, 221, 11))),
                                    Comment(" comment 95", Some(AstLocation(3153, 222, 11)))),
                                  Vector.empty,
                                  Some(AstLocation(3176, 223, 11))
                                ),
                                FragmentSpread(
                                  "frag",
                                  Vector.empty,
                                  Vector(
                                    Comment(" comment 96", Some(AstLocation(3190, 224, 11))),
                                    Comment(" comment 97", Some(AstLocation(3213, 225, 11)))),
                                  Some(AstLocation(3237, 227, 11))
                                )
                              ),
                              Vector(
                                Comment(" comment 58", Some(AstLocation(2151, 167, 7))),
                                Comment(" comment 59", Some(AstLocation(2170, 168, 7)))),
                              Vector(
                                Comment(" comment 100", Some(AstLocation(3312, 231, 11))),
                                Comment(" comment 101", Some(AstLocation(3336, 232, 11)))),
                              Some(AstLocation(2191, 169, 9))
                            )),
                          Vector.empty,
                          Vector(
                            Comment(" comment 102", Some(AstLocation(3368, 234, 9))),
                            Comment(" comment 103", Some(AstLocation(3390, 235, 9)))),
                          Some(AstLocation(2092, 163, 7))
                        )),
                      Vector(
                        Comment(" comment 46", Some(AstLocation(1879, 148, 5))),
                        Comment(" comment 47", Some(AstLocation(1896, 149, 5)))),
                      Vector(
                        Comment(" comment 104", Some(AstLocation(3418, 237, 7))),
                        Comment(" comment 105", Some(AstLocation(3438, 238, 7)))),
                      Some(AstLocation(1913, 150, 5))
                    )
                  ),
                  Vector(
                    Comment(" comment 21", Some(AstLocation(1408, 109, 2))),
                    Comment(" comment 22", Some(AstLocation(1422, 110, 2)))),
                  Vector(
                    Comment(" comment 106", Some(AstLocation(3462, 240, 5))),
                    Comment(" comment 107", Some(AstLocation(3480, 241, 5)))),
                  Some(AstLocation(1437, 111, 3))
                )),
              Vector(
                Comment(" Copyright (c) 2015, Facebook, Inc.", Some(AstLocation(0, 1, 1))),
                Comment(" All rights reserved.", Some(AstLocation(37, 2, 1))),
                Comment("", Some(AstLocation(60, 3, 1))),
                Comment(
                  " This source code is licensed under the BSD-style license found in the",
                  Some(AstLocation(62, 4, 1))),
                Comment(
                  " LICENSE file in the root directory of this source tree. An additional grant",
                  Some(AstLocation(134, 5, 1))),
                Comment(
                  " of patent rights can be found in the PATENTS file in the same directory.",
                  Some(AstLocation(212, 6, 1)))
              ),
              Vector(
                Comment(" comment 108", Some(AstLocation(3500, 243, 3))),
                Comment(" comment 109", Some(AstLocation(3516, 244, 3)))),
              Some(AstLocation(288, 8, 1))
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
                      BigIntValue(
                        123,
                        Vector(
                          Comment(" comment 124", Some(AstLocation(3793, 268, 3))),
                          Comment(" comment 125", Some(AstLocation(3809, 269, 3)))),
                        Some(AstLocation(3825, 270, 3))
                      ),
                      Vector(
                        Comment(" comment 120", Some(AstLocation(3717, 262, 3))),
                        Comment(" comment 121", Some(AstLocation(3733, 263, 3)))),
                      Some(AstLocation(3749, 264, 3))
                    )),
                  Vector(Directive(
                    "defer",
                    Vector.empty,
                    Vector(
                      Comment(" comment 128", Some(AstLocation(3867, 274, 3))),
                      Comment(" comment 129", Some(AstLocation(3883, 275, 3)))),
                    Some(AstLocation(3899, 276, 3))
                  )),
                  Vector(
                    Field(
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
                          Vector(
                            Comment(" comment 136", Some(AstLocation(4030, 286, 5))),
                            Comment(" comment 137", Some(AstLocation(4048, 287, 5))),
                            Comment(" comment 138", Some(AstLocation(4067, 289, 5))),
                            Comment(" comment 139", Some(AstLocation(4085, 290, 5)))
                          ),
                          Vector.empty,
                          Some(AstLocation(4105, 291, 7))
                        )),
                      Vector(
                        Comment(" comment 132", Some(AstLocation(3944, 280, 3))),
                        Comment(" comment 133", Some(AstLocation(3960, 281, 3)))),
                      Vector(
                        Comment(" comment 140", Some(AstLocation(4114, 292, 7))),
                        Comment(" comment 141", Some(AstLocation(4134, 293, 7)))),
                      Some(AstLocation(3978, 282, 5))
                    )),
                  Vector(
                    Comment(" comment 116", Some(AstLocation(3644, 256, 1))),
                    Comment(" comment 117", Some(AstLocation(3658, 257, 1)))),
                  Vector(
                    Comment(" comment 142", Some(AstLocation(4158, 295, 5))),
                    Comment(" comment 143", Some(AstLocation(4176, 296, 5)))),
                  Some(AstLocation(3674, 258, 3))
                )),
              Vector(
                Comment(" comment 110", Some(AstLocation(3536, 247, 4))),
                Comment(" comment 111", Some(AstLocation(3553, 248, 4)))),
              Vector(
                Comment(" comment 144", Some(AstLocation(4196, 298, 3))),
                Comment(" comment 145", Some(AstLocation(4212, 299, 3)))),
              Some(AstLocation(3567, 249, 1))
            ),
            FragmentDefinition(
              "frag",
              NamedType("Friend", Some(AstLocation(4358, 312, 1))),
              Vector.empty,
              Vector(
                InlineFragment(
                  None,
                  Vector(
                    Directive(
                      "skip",
                      Vector(
                        Argument(
                          "unless",
                          VariableValue(
                            "foo",
                            Vector(
                              Comment(" comment 168", Some(AstLocation(4613, 334, 3))),
                              Comment(" comment 169", Some(AstLocation(4629, 335, 3)))),
                            Some(AstLocation(4645, 336, 3))
                          ),
                          Vector(
                            Comment(" comment 164", Some(AstLocation(4536, 328, 3))),
                            Comment(" comment 165", Some(AstLocation(4552, 329, 3)))),
                          Some(AstLocation(4568, 330, 3))
                        )),
                      Vector(
                        Comment(" comment 160", Some(AstLocation(4460, 322, 3))),
                        Comment(" comment 161", Some(AstLocation(4476, 323, 3)))),
                      Some(AstLocation(4492, 324, 3))
                    )),
                  Vector(Field(
                    None,
                    "id",
                    Vector.empty,
                    Vector.empty,
                    Vector.empty,
                    Vector(
                      Comment(" comment 174", Some(AstLocation(4724, 343, 3))),
                      Comment(" comment 175", Some(AstLocation(4740, 344, 3)))),
                    Vector.empty,
                    Some(AstLocation(4758, 345, 5))
                  )),
                  Vector(
                    Comment(" comment 156", Some(AstLocation(4395, 316, 1))),
                    Comment(" comment 157", Some(AstLocation(4409, 317, 1))),
                    Comment(" comment 158", Some(AstLocation(4424, 319, 1))),
                    Comment(" comment 159", Some(AstLocation(4438, 320, 1)))
                  ),
                  Vector(
                    Comment(" comment 176", Some(AstLocation(4765, 346, 5))),
                    Comment(" comment 177", Some(AstLocation(4783, 347, 5)))),
                  Some(AstLocation(4454, 321, 3))
                )),
              Vector.empty,
              Vector(
                Comment(" comment 146", Some(AstLocation(4228, 300, 3))),
                Comment(" comment 147", Some(AstLocation(4242, 301, 1)))),
              Vector(
                Comment(" comment 178", Some(AstLocation(4803, 349, 3))),
                Comment(" comment 179", Some(AstLocation(4819, 350, 3)))),
              Some(AstLocation(4257, 303, 1))
            )
          ),
          Vector(
            Comment(" comment 180", Some(AstLocation(4835, 352, 1))),
            Comment(" comment 181", Some(AstLocation(4849, 353, 1)))),
          Some(AstLocation(0, 1, 1)),
          None
        )

      QueryParser.parse(
        query,
        ParserConfig.default.withEmptySourceId.withoutSourceMapper) should be(Success(expected))
    }

    "parse document with block strings" in {
      val query = FileUtil.loadQuery("block-string.graphql")

      val expected = Document(
        Vector(
          OperationDefinition(
            OperationType.Query,
            Some("FetchLukeAndLeiaAliased"),
            Vector(
              VariableDefinition(
                "someVar",
                NamedType("String", Some(AstLocation(40, 1, 41))),
                Some(
                  StringValue(
                    "hello \\\n  world",
                    true,
                    Some("\n    hello \\\n      world"),
                    Vector.empty,
                    Some(AstLocation(53, 2, 5)))),
                Vector.empty,
                Vector.empty,
                Some(AstLocation(30, 1, 31))
              )),
            Vector.empty,
            Vector(
              Field(
                Some("luke"),
                "human",
                Vector(
                  Argument(
                    "id",
                    StringValue("1000", false, None, Vector.empty, Some(AstLocation(105, 5, 19))),
                    Vector.empty,
                    Some(AstLocation(101, 5, 15))
                  ),
                  Argument(
                    "bar",
                    StringValue(
                      " \\\"test\n123 \\u0000",
                      true,
                      Some(" \\\"test\n     123 \\u0000\n     "),
                      Vector.empty,
                      Some(AstLocation(118, 5, 32))),
                    Vector.empty,
                    Some(AstLocation(113, 5, 27))
                  )
                ),
                Vector.empty,
                Vector.empty,
                Vector.empty,
                Vector.empty,
                Some(AstLocation(89, 5, 3))
              ),
              FragmentSpread("Foo", Vector.empty, Vector.empty, Some(AstLocation(158, 9, 3)))
            ),
            Vector.empty,
            Vector.empty,
            Some(AstLocation(0, 1, 1))
          ),
          FragmentDefinition(
            "Foo",
            NamedType("User", Some(AstLocation(184, 12, 17))),
            Vector(
              Directive(
                "foo",
                Vector(
                  Argument(
                    "bar",
                    BigIntValue(1, Vector.empty, Some(AstLocation(199, 12, 32))),
                    Vector.empty,
                    Some(AstLocation(194, 12, 27))
                  )),
                Vector.empty,
                Some(AstLocation(189, 12, 22))
              )),
            Vector(
              Field(
                None,
                "baz",
                Vector.empty,
                Vector(
                  Directive(
                    "docs",
                    Vector(Argument(
                      "info",
                      StringValue(
                        "\"\"\"\n\"\"\" this \" is \"\"\na description! \"\"\"",
                        true,
                        Some("\"\"\"\n    \"\"\" this \" is \"\"\n    a description! \"\"\"\n    "),
                        Vector.empty,
                        Some(AstLocation(225, 14, 5))
                      ),
                      Vector.empty,
                      Some(AstLocation(215, 13, 13))
                    )),
                    Vector.empty,
                    Some(AstLocation(209, 13, 7))
                  )),
                Vector.empty,
                Vector.empty,
                Vector.empty,
                Some(AstLocation(205, 13, 3))
              )),
            Vector.empty,
            Vector.empty,
            Vector.empty,
            Some(AstLocation(168, 12, 1))
          )
        ),
        Vector.empty,
        Some(AstLocation(0, 1, 1)),
        None
      )

      parseQuery(query) should be(Success(expected))
    }

    "Experimental: allows parsing fragment defined variables" in {
      val queryStr = "fragment a($v: Boolean = false) on t { f(v: $v) }"

      parseQuery(queryStr).isFailure should be(true)

      val query = QueryParser
        .parse(
          queryStr,
          ParserConfig.default.withEmptySourceId.withoutSourceMapper.withExperimentalFragmentVariables)
        .get

      query should be(
        Document(
          Vector(
            FragmentDefinition(
              "a",
              NamedType("t", Some(AstLocation(35, 1, 36))),
              Vector.empty,
              Vector(Field(
                None,
                "f",
                Vector(
                  Argument(
                    "v",
                    VariableValue("v", Vector.empty, Some(AstLocation(44, 1, 45))),
                    Vector.empty,
                    Some(AstLocation(41, 1, 42))
                  )),
                Vector.empty,
                Vector.empty,
                Vector.empty,
                Vector.empty,
                Some(AstLocation(39, 1, 40))
              )),
              Vector(VariableDefinition(
                "v",
                NamedType("Boolean", Some(AstLocation(15, 1, 16))),
                Some(BooleanValue(false, Vector.empty, Some(AstLocation(25, 1, 26)))),
                Vector.empty,
                Vector.empty,
                Some(AstLocation(11, 1, 12))
              )),
              Vector.empty,
              Vector.empty,
              Some(AstLocation(0, 1, 1))
            )),
          Vector.empty,
          Some(AstLocation(0, 1, 1)),
          None
        ))
    }
  }

  "Ast" should {
    "be equal for the same queries" in {
      val query =
        """
          {
            id
            name
            friends {
              name
            }
          }
        """

      (parseQuery(query) == parseQuery(query)) should be(true)
    }

    "not be equal for the same queries with different AST node positions" in {
      val query1 =
        """
          {
            id
            name
            friends {
              name
            }
          }
        """

      val query2 =
        """
          {
            id
            name
            friends {name}
          }
        """

      (parseQuery(query1) == parseQuery(query2)) should be(false)
    }
  }
}
