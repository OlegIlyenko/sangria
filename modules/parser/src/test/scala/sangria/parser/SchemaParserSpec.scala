package sangria.parser

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import sangria.ast._
import sangria.util.{FileUtil, StringMatchers}

import scala.util.Success

class SchemaParserSpec extends AnyWordSpec with Matchers with StringMatchers {
  private[this] def parseQuery(query: String) =
    QueryParser.parse(query, ParserConfig.default.withEmptySourceId.withoutSourceMapper)

  "QueryParser" should {
    "parse schema kitchen sink" in {
      val query = FileUtil.loadQuery("schema-kitchen-sink.graphql")

      val expectedAst =
        Document(
          Vector(
            SchemaDefinition(
              Vector(
                OperationTypeDefinition(
                  OperationType.Query,
                  NamedType("QueryType", Some(AstLocation(306, 9, 10))),
                  Vector.empty,
                  Some(AstLocation(299, 9, 3))),
                OperationTypeDefinition(
                  OperationType.Mutation,
                  NamedType("MutationType", Some(AstLocation(328, 10, 13))),
                  Vector.empty,
                  Some(AstLocation(318, 10, 3)))
              ),
              Vector.empty,
              None,
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
            ObjectTypeDefinition(
              "Foo",
              Vector(NamedType("Bar", Some(AstLocation(390, 16, 21)))),
              Vector(
                FieldDefinition(
                  "one",
                  NamedType("Type", Some(AstLocation(403, 17, 8))),
                  Vector.empty,
                  Vector.empty,
                  None,
                  Vector.empty,
                  Some(AstLocation(398, 17, 3))),
                FieldDefinition(
                  "two",
                  NamedType("Type", Some(AstLocation(437, 18, 30))),
                  Vector(InputValueDefinition(
                    "argument",
                    NotNullType(
                      NamedType("InputType", Some(AstLocation(424, 18, 17))),
                      Some(AstLocation(424, 18, 17))),
                    None,
                    Vector.empty,
                    None,
                    Vector.empty,
                    Some(AstLocation(414, 18, 7))
                  )),
                  Vector.empty,
                  None,
                  Vector.empty,
                  Some(AstLocation(410, 18, 3))
                ),
                FieldDefinition(
                  "three",
                  NamedType("Int", Some(AstLocation(487, 19, 46))),
                  Vector(
                    InputValueDefinition(
                      "argument",
                      NamedType("InputType", Some(AstLocation(460, 19, 19))),
                      None,
                      Vector.empty,
                      None,
                      Vector.empty,
                      Some(AstLocation(450, 19, 9))),
                    InputValueDefinition(
                      "other",
                      NamedType("String", Some(AstLocation(478, 19, 37))),
                      None,
                      Vector.empty,
                      None,
                      Vector.empty,
                      Some(AstLocation(471, 19, 30)))
                  ),
                  Vector.empty,
                  None,
                  Vector.empty,
                  Some(AstLocation(444, 19, 3))
                ),
                FieldDefinition(
                  "four",
                  NamedType("String", Some(AstLocation(528, 20, 38))),
                  Vector(InputValueDefinition(
                    "argument",
                    NamedType("String", Some(AstLocation(508, 20, 18))),
                    Some(
                      StringValue(
                        "string",
                        false,
                        None,
                        Vector.empty,
                        Some(AstLocation(517, 20, 27)))),
                    Vector.empty,
                    None,
                    Vector.empty,
                    Some(AstLocation(498, 20, 8))
                  )),
                  Vector.empty,
                  None,
                  Vector.empty,
                  Some(AstLocation(493, 20, 3))
                ),
                FieldDefinition(
                  "five",
                  NamedType("String", Some(AstLocation(586, 21, 52))),
                  Vector(
                    InputValueDefinition(
                      "argument",
                      ListType(
                        NamedType("String", Some(AstLocation(553, 21, 19))),
                        Some(AstLocation(552, 21, 18))),
                      Some(ListValue(
                        Vector(
                          StringValue(
                            "string",
                            false,
                            None,
                            Vector.empty,
                            Some(AstLocation(564, 21, 30))),
                          StringValue(
                            "string",
                            false,
                            None,
                            Vector.empty,
                            Some(AstLocation(574, 21, 40)))
                        ),
                        Vector.empty,
                        Some(AstLocation(563, 21, 29))
                      )),
                      Vector.empty,
                      None,
                      Vector.empty,
                      Some(AstLocation(542, 21, 8))
                    )),
                  Vector.empty,
                  None,
                  Vector.empty,
                  Some(AstLocation(537, 21, 3))
                ),
                FieldDefinition(
                  "six",
                  NamedType("Type", Some(AstLocation(678, 26, 46))),
                  Vector(
                    InputValueDefinition(
                      "argument",
                      NamedType("InputType", Some(AstLocation(649, 26, 17))),
                      Some(ObjectValue(
                        Vector(
                          ObjectField(
                            "key",
                            StringValue(
                              "value",
                              false,
                              None,
                              Vector.empty,
                              Some(AstLocation(667, 26, 35))),
                            Vector.empty,
                            Some(AstLocation(662, 26, 30))
                          )),
                        Vector.empty,
                        Some(AstLocation(661, 26, 29))
                      )),
                      Vector.empty,
                      None,
                      Vector.empty,
                      Some(AstLocation(639, 26, 7))
                    )),
                  Vector.empty,
                  Some(
                    StringValue(
                      "More \"\"\" descriptions \\",
                      true,
                      Some("\n  More \"\"\" descriptions \\\n  "),
                      Vector.empty,
                      Some(AstLocation(596, 23, 3)))),
                  Vector.empty,
                  Some(AstLocation(635, 26, 3))
                )
              ),
              Vector.empty,
              Some(
                StringValue(
                  "type description!",
                  true,
                  Some("\ntype description!\n"),
                  Vector.empty,
                  Some(AstLocation(344, 13, 1)))),
              Vector.empty,
              Vector.empty,
              Some(AstLocation(370, 16, 1))
            ),
            ObjectTypeDefinition(
              "AnnotatedObject",
              Vector.empty,
              Vector(
                FieldDefinition(
                  "annotatedField",
                  NamedType("Type", Some(AstLocation(781, 30, 49))),
                  Vector(InputValueDefinition(
                    "arg",
                    NamedType("Type", Some(AstLocation(755, 30, 23))),
                    Some(
                      StringValue(
                        "default",
                        false,
                        None,
                        Vector.empty,
                        Some(AstLocation(762, 30, 30)))),
                    Vector(
                      Directive(
                        "onArg",
                        Vector.empty,
                        Vector.empty,
                        Some(AstLocation(772, 30, 40))
                      )),
                    None,
                    Vector.empty,
                    Some(AstLocation(750, 30, 18))
                  )),
                  Vector(
                    Directive(
                      "onField",
                      Vector.empty,
                      Vector.empty,
                      Some(AstLocation(786, 30, 54))
                    )),
                  None,
                  Vector.empty,
                  Some(AstLocation(735, 30, 3))
                )),
              Vector(
                Directive(
                  "onObject",
                  Vector(Argument(
                    "arg",
                    StringValue("value", false, None, Vector.empty, Some(AstLocation(722, 29, 37))),
                    Vector.empty,
                    Some(AstLocation(717, 29, 32))
                  )),
                  Vector.empty,
                  Some(AstLocation(707, 29, 22))
                )),
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(686, 29, 1))
            ),
            InterfaceTypeDefinition(
              "Bar",
              Vector(
                FieldDefinition(
                  "one",
                  NamedType("Type", Some(AstLocation(875, 37, 8))),
                  Vector.empty,
                  Vector.empty,
                  None,
                  Vector.empty,
                  Some(AstLocation(870, 37, 3))),
                FieldDefinition(
                  "four",
                  NamedType("String", Some(AstLocation(917, 38, 38))),
                  Vector(InputValueDefinition(
                    "argument",
                    NamedType("String", Some(AstLocation(897, 38, 18))),
                    Some(
                      StringValue(
                        "string",
                        false,
                        None,
                        Vector.empty,
                        Some(AstLocation(906, 38, 27)))),
                    Vector.empty,
                    None,
                    Vector.empty,
                    Some(AstLocation(887, 38, 8))
                  )),
                  Vector.empty,
                  None,
                  Vector.empty,
                  Some(AstLocation(882, 38, 3))
                )
              ),
              Vector.empty,
              Some(
                StringValue(
                  " It's an interface!",
                  false,
                  None,
                  Vector(Comment(" comment above", Some(AstLocation(798, 33, 1)))),
                  Some(AstLocation(814, 34, 1)))),
              Vector(Comment(" comment below", Some(AstLocation(836, 35, 1)))),
              Vector.empty,
              Some(AstLocation(852, 36, 1))
            ),
            InterfaceTypeDefinition(
              "AnnotatedInterface",
              Vector(
                FieldDefinition(
                  "annotatedField",
                  NamedType("Type", Some(AstLocation(1007, 42, 37))),
                  Vector(InputValueDefinition(
                    "arg",
                    NamedType("Type", Some(AstLocation(993, 42, 23))),
                    None,
                    Vector(
                      Directive(
                        "onArg",
                        Vector.empty,
                        Vector.empty,
                        Some(AstLocation(998, 42, 28))
                      )),
                    None,
                    Vector.empty,
                    Some(AstLocation(988, 42, 18))
                  )),
                  Vector(
                    Directive(
                      "onField",
                      Vector.empty,
                      Vector.empty,
                      Some(AstLocation(1012, 42, 42))
                    )),
                  None,
                  Vector.empty,
                  Some(AstLocation(973, 42, 3))
                )),
              Vector(
                Directive(
                  "onInterface",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(956, 41, 30))
                )),
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(927, 41, 1))
            ),
            UnionTypeDefinition(
              "Feed",
              Vector(
                NamedType("Story", Some(AstLocation(1037, 45, 14))),
                NamedType("Article", Some(AstLocation(1045, 45, 22))),
                NamedType("Advert", Some(AstLocation(1055, 45, 32)))
              ),
              Vector.empty,
              None,
              Vector.empty,
              Some(AstLocation(1024, 45, 1))
            ),
            UnionTypeDefinition(
              "AnnotatedUnion",
              Vector(
                NamedType("A", Some(AstLocation(1095, 47, 33))),
                NamedType("B", Some(AstLocation(1099, 47, 37)))),
              Vector(
                Directive(
                  "onUnion",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(1084, 47, 22))
                )),
              None,
              Vector.empty,
              Some(AstLocation(1063, 47, 1))
            ),
            ScalarTypeDefinition(
              "CustomScalar",
              Vector.empty,
              None,
              Vector.empty,
              Some(AstLocation(1102, 49, 1))
            ),
            ScalarTypeDefinition(
              "AnnotatedScalar",
              Vector(
                Directive(
                  "onScalar",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(1146, 51, 24))
                )),
              None,
              Vector.empty,
              Some(AstLocation(1123, 51, 1))
            ),
            EnumTypeDefinition(
              "Site",
              Vector(
                EnumValueDefinition(
                  "DESKTOP",
                  Vector.empty,
                  Some(
                    StringValue(
                      "description 1",
                      false,
                      None,
                      Vector.empty,
                      Some(AstLocation(1171, 54, 3)))),
                  Vector.empty,
                  Some(AstLocation(1189, 55, 3))
                ),
                EnumValueDefinition(
                  "MOBILE",
                  Vector.empty,
                  Some(
                    StringValue(
                      "description 2",
                      true,
                      Some("\n  description 2\n  "),
                      Vector.empty,
                      Some(AstLocation(1199, 56, 3)))),
                  Vector.empty,
                  Some(AstLocation(1227, 59, 3))
                )
              ),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(1157, 53, 1))
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
                      Some(AstLocation(1284, 63, 19))
                    )),
                  None,
                  Vector.empty,
                  Some(AstLocation(1268, 63, 3))
                ),
                EnumValueDefinition(
                  "OTHER_VALUE",
                  Vector.empty,
                  None,
                  Vector.empty,
                  Some(AstLocation(1299, 64, 3)))
              ),
              Vector(
                Directive(
                  "onEnum",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(1256, 62, 20))
                )),
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(1237, 62, 1))
            ),
            InputObjectTypeDefinition(
              "InputType",
              Vector(
                InputValueDefinition(
                  "key",
                  NotNullType(
                    NamedType("String", Some(AstLocation(1339, 68, 8))),
                    Some(AstLocation(1339, 68, 8))),
                  None,
                  Vector.empty,
                  None,
                  Vector.empty,
                  Some(AstLocation(1334, 68, 3))
                ),
                InputValueDefinition(
                  "answer",
                  NamedType("Int", Some(AstLocation(1357, 69, 11))),
                  Some(BigIntValue(42, Vector.empty, Some(AstLocation(1363, 69, 17)))),
                  Vector.empty,
                  None,
                  Vector.empty,
                  Some(AstLocation(1349, 69, 3))
                )
              ),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(1314, 67, 1))
            ),
            InputObjectTypeDefinition(
              "AnnotatedInput",
              Vector(
                InputValueDefinition(
                  "annotatedField",
                  NamedType("Type", Some(AstLocation(1447, 74, 19))),
                  None,
                  Vector(
                    Directive(
                      "onField",
                      Vector.empty,
                      Vector.empty,
                      Some(AstLocation(1452, 74, 24))
                    )),
                  None,
                  Vector(Comment(" field comment", Some(AstLocation(1413, 73, 3)))),
                  Some(AstLocation(1431, 74, 3))
                )),
              Vector(
                Directive(
                  "onInputObjectType",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(1390, 72, 22))
                )),
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(1369, 72, 1))
            ),
            ObjectTypeExtensionDefinition(
              "Foo",
              Vector.empty,
              Vector(
                FieldDefinition(
                  "seven",
                  NamedType("Type", Some(AstLocation(1511, 78, 30))),
                  Vector(InputValueDefinition(
                    "argument",
                    ListType(
                      NamedType("String", Some(AstLocation(1501, 78, 20))),
                      Some(AstLocation(1500, 78, 19))),
                    None,
                    Vector.empty,
                    None,
                    Vector.empty,
                    Some(AstLocation(1490, 78, 9))
                  )),
                  Vector.empty,
                  None,
                  Vector.empty,
                  Some(AstLocation(1484, 78, 3))
                )),
              Vector.empty,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(1464, 77, 1))
            ),
            ObjectTypeExtensionDefinition(
              "Foo",
              Vector.empty,
              Vector.empty,
              Vector(
                Directive(
                  "onType",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(1535, 81, 17))
                )),
              Vector.empty,
              Vector.empty,
              Some(AstLocation(1519, 81, 1))
            ),
            DirectiveDefinition(
              "skip",
              Vector(
                InputValueDefinition(
                  "if",
                  NotNullType(
                    NamedType("Boolean", Some(AstLocation(1577, 84, 21))),
                    Some(AstLocation(1577, 84, 21))),
                  None,
                  Vector.empty,
                  None,
                  Vector.empty,
                  Some(AstLocation(1573, 84, 17))
                )),
              Vector(
                DirectiveLocation("FIELD", Vector.empty, Some(AstLocation(1590, 84, 34))),
                DirectiveLocation("FRAGMENT_SPREAD", Vector.empty, Some(AstLocation(1598, 84, 42))),
                DirectiveLocation("INLINE_FRAGMENT", Vector.empty, Some(AstLocation(1616, 84, 60)))
              ),
              Some(
                StringValue(
                  "cool skip",
                  false,
                  None,
                  Vector.empty,
                  Some(AstLocation(1545, 83, 1)))),
              false,
              Vector.empty,
              Some(AstLocation(1557, 84, 1))
            ),
            DirectiveDefinition(
              "include",
              Vector(
                InputValueDefinition(
                  "if",
                  NotNullType(
                    NamedType("Boolean", Some(AstLocation(1656, 86, 24))),
                    Some(AstLocation(1656, 86, 24))),
                  None,
                  Vector.empty,
                  None,
                  Vector.empty,
                  Some(AstLocation(1652, 86, 20))
                )),
              Vector(
                DirectiveLocation("FIELD", Vector.empty, Some(AstLocation(1671, 87, 6))),
                DirectiveLocation("FRAGMENT_SPREAD", Vector.empty, Some(AstLocation(1682, 88, 6))),
                DirectiveLocation("INLINE_FRAGMENT", Vector.empty, Some(AstLocation(1703, 89, 6)))
              ),
              None,
              false,
              Vector.empty,
              Some(AstLocation(1633, 86, 1))
            )
          ),
          Vector.empty,
          Some(AstLocation(0, 1, 1)),
          None
        )

      parseQuery(query) should be(Success(expectedAst))
    }

    "Simple type" in {
      val ast = parseQuery("""
          # my type
          # comment
          type Hello {
            # and field comment as well
            world: String
          }
        """).get

      ast.withoutSourceMapper should be(
        Document(
          Vector(
            ObjectTypeDefinition(
              "Hello",
              Vector.empty,
              Vector(FieldDefinition(
                "world",
                NamedType("String", Some(AstLocation(123, 6, 20))),
                Vector.empty,
                Vector.empty,
                None,
                Vector(Comment(" and field comment as well", Some(AstLocation(76, 5, 13)))),
                Some(AstLocation(116, 6, 13))
              )),
              Vector.empty,
              None,
              Vector(
                Comment(" my type", Some(AstLocation(11, 2, 11))),
                Comment(" comment", Some(AstLocation(31, 3, 11)))),
              Vector.empty,
              Some(AstLocation(51, 4, 11))
            )),
          Vector.empty,
          Some(AstLocation(11, 2, 11)),
          None
        ))
    }

    "Simple extension" in {
      val ast = parseQuery("""
          # my type
          # comment
          extend type Hello {
            world: String
          }
        """).get

      ast.withoutSourceMapper should be(
        Document(
          Vector(
            ObjectTypeExtensionDefinition(
              "Hello",
              Vector.empty,
              Vector(
                FieldDefinition(
                  "world",
                  NamedType("String", Some(AstLocation(90, 5, 20))),
                  Vector.empty,
                  Vector.empty,
                  None,
                  Vector.empty,
                  Some(AstLocation(83, 5, 13)))),
              Vector.empty,
              Vector(
                Comment(" my type", Some(AstLocation(11, 2, 11))),
                Comment(" comment", Some(AstLocation(31, 3, 11)))),
              Vector.empty,
              Some(AstLocation(51, 4, 11))
            )),
          Vector.empty,
          Some(AstLocation(11, 2, 11)),
          None
        ))
    }

    "Simple non-null type" in {
      val ast = parseQuery("""
          type Hello {
            world: String!
          }
        """).get

      ast.withoutSourceMapper should be(
        Document(
          Vector(ObjectTypeDefinition(
            "Hello",
            Vector.empty,
            Vector(FieldDefinition(
              "world",
              NotNullType(
                NamedType("String", Some(AstLocation(43, 3, 20))),
                Some(AstLocation(43, 3, 20))),
              Vector.empty,
              Vector.empty,
              None,
              Vector.empty,
              Some(AstLocation(36, 3, 13))
            )),
            Vector.empty,
            None,
            Vector.empty,
            Vector.empty,
            Some(AstLocation(11, 2, 11))
          )),
          Vector.empty,
          Some(AstLocation(11, 2, 11)),
          None
        ))
    }

    "Simple type inheriting interface" in {
      val ast = parseQuery("type Hello implements World { foo: Bar }").get

      ast.withoutSourceMapper should be(
        Document(
          Vector(ObjectTypeDefinition(
            "Hello",
            Vector(NamedType("World", Some(AstLocation(22, 1, 23)))),
            Vector(
              FieldDefinition(
                "foo",
                NamedType("Bar", Some(AstLocation(35, 1, 36))),
                Vector.empty,
                Vector.empty,
                None,
                Vector.empty,
                Some(AstLocation(30, 1, 31)))),
            Vector.empty,
            None,
            Vector.empty,
            Vector.empty,
            Some(AstLocation(0, 1, 1))
          )),
          Vector.empty,
          Some(AstLocation(0, 1, 1)),
          None
        ))
    }

    "Simple type inheriting multiple interfaces" in {
      val ast = parseQuery("type Hello implements Wo & rld { foo: Bar }").get

      ast.withoutSourceMapper should be(
        Document(
          Vector(
            ObjectTypeDefinition(
              "Hello",
              Vector(
                NamedType("Wo", Some(AstLocation(22, 1, 23))),
                NamedType("rld", Some(AstLocation(27, 1, 28)))),
              Vector(
                FieldDefinition(
                  "foo",
                  NamedType("Bar", Some(AstLocation(38, 1, 39))),
                  Vector.empty,
                  Vector.empty,
                  None,
                  Vector.empty,
                  Some(AstLocation(33, 1, 34)))),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(0, 1, 1))
            )),
          Vector.empty,
          Some(AstLocation(0, 1, 1)),
          None
        ))
    }

    "Simple type inheriting multiple interfaces (allow separator at the beginning)" in {
      val ast = parseQuery("""
          type Hello implements
            & Foo
            & Baz
          {
            foo: Bar
          }
        """).get

      ast.withoutSourceMapper should be(
        Document(
          Vector(
            ObjectTypeDefinition(
              "Hello",
              Vector(
                NamedType("Foo", Some(AstLocation(47, 3, 15))),
                NamedType("Baz", Some(AstLocation(65, 4, 15)))),
              Vector(
                FieldDefinition(
                  "foo",
                  NamedType("Bar", Some(AstLocation(98, 6, 18))),
                  Vector.empty,
                  Vector.empty,
                  None,
                  Vector.empty,
                  Some(AstLocation(93, 6, 13)))),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(11, 2, 11))
            )),
          Vector.empty,
          Some(AstLocation(11, 2, 11)),
          None
        ))
    }

    "Double value enum" in {
      val ast = parseQuery("enum Hello { WO, RLD }").get

      ast.withoutSourceMapper should be(
        Document(
          Vector(EnumTypeDefinition(
            "Hello",
            Vector(
              EnumValueDefinition(
                "WO",
                Vector.empty,
                None,
                Vector.empty,
                Some(AstLocation(13, 1, 14))),
              EnumValueDefinition(
                "RLD",
                Vector.empty,
                None,
                Vector.empty,
                Some(AstLocation(17, 1, 18)))
            ),
            Vector.empty,
            None,
            Vector.empty,
            Vector.empty,
            Some(AstLocation(0, 1, 1))
          )),
          Vector.empty,
          Some(AstLocation(0, 1, 1)),
          None
        ))
    }

    "Simple interface" in {
      val ast = parseQuery("""
          #foo
          interface Hello {
            world: String
          }
        """).get

      ast.withoutSourceMapper should be(
        Document(
          Vector(InterfaceTypeDefinition(
            "Hello",
            Vector(
              FieldDefinition(
                "world",
                NamedType("String", Some(AstLocation(63, 4, 20))),
                Vector.empty,
                Vector.empty,
                None,
                Vector.empty,
                Some(AstLocation(56, 4, 13)))),
            Vector.empty,
            None,
            Vector(Comment("foo", Some(AstLocation(11, 2, 11)))),
            Vector.empty,
            Some(AstLocation(26, 3, 11))
          )),
          Vector.empty,
          Some(AstLocation(11, 2, 11)),
          None
        ))
    }

    "Simple field with arg" in {
      val ast = parseQuery("""
          #c1
          type Hello {
            #c2
            world(
              #c3

              #c4
              flag: Boolean): String
          }
        """).get

      ast.withoutSourceMapper should be(
        Document(
          Vector(
            ObjectTypeDefinition(
              "Hello",
              Vector.empty,
              Vector(
                FieldDefinition(
                  "world",
                  NamedType("String", Some(AstLocation(140, 9, 31))),
                  Vector(InputValueDefinition(
                    "flag",
                    NamedType("Boolean", Some(AstLocation(130, 9, 21))),
                    None,
                    Vector.empty,
                    None,
                    Vector(
                      Comment("c3", Some(AstLocation(87, 6, 15))),
                      Comment("c4", Some(AstLocation(106, 8, 15)))),
                    Some(AstLocation(124, 9, 15))
                  )),
                  Vector.empty,
                  None,
                  Vector(Comment("c2", Some(AstLocation(50, 4, 13)))),
                  Some(AstLocation(66, 5, 13))
                )),
              Vector.empty,
              None,
              Vector(Comment("c1", Some(AstLocation(11, 2, 11)))),
              Vector.empty,
              Some(AstLocation(25, 3, 11))
            )),
          Vector.empty,
          Some(AstLocation(11, 2, 11)),
          None
        ))
    }

    "Simple field with arg with default value" in {
      val ast = parseQuery("""
          type Hello {
            world(flag: Boolean =
              # value comment
              true): String
          }
        """).get

      ast.withoutSourceMapper should be(
        Document(
          Vector(
            ObjectTypeDefinition(
              "Hello",
              Vector.empty,
              Vector(
                FieldDefinition(
                  "world",
                  NamedType("String", Some(AstLocation(109, 5, 22))),
                  Vector(InputValueDefinition(
                    "flag",
                    NamedType("Boolean", Some(AstLocation(48, 3, 25))),
                    Some(BooleanValue(
                      true,
                      Vector(Comment(" value comment", Some(AstLocation(72, 4, 15)))),
                      Some(AstLocation(102, 5, 15)))),
                    Vector.empty,
                    None,
                    Vector.empty,
                    Some(AstLocation(42, 3, 19))
                  )),
                  Vector.empty,
                  None,
                  Vector.empty,
                  Some(AstLocation(36, 3, 13))
                )),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(11, 2, 11))
            )),
          Vector.empty,
          Some(AstLocation(11, 2, 11)),
          None
        ))
    }

    "Simple field with list arg" in {
      val ast = parseQuery("""
          type Hello {
            world(things: [String]): String
          }
        """).get

      ast.withoutSourceMapper should be(
        Document(
          Vector(
            ObjectTypeDefinition(
              "Hello",
              Vector.empty,
              Vector(FieldDefinition(
                "world",
                NamedType("String", Some(AstLocation(61, 3, 38))),
                Vector(InputValueDefinition(
                  "things",
                  ListType(
                    NamedType("String", Some(AstLocation(51, 3, 28))),
                    Some(AstLocation(50, 3, 27))),
                  None,
                  Vector.empty,
                  None,
                  Vector.empty,
                  Some(AstLocation(42, 3, 19))
                )),
                Vector.empty,
                None,
                Vector.empty,
                Some(AstLocation(36, 3, 13))
              )),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(11, 2, 11))
            )),
          Vector.empty,
          Some(AstLocation(11, 2, 11)),
          None
        ))
    }

    "Simple field with two args" in {
      val ast = parseQuery("""
          type Hello {
            world(argOne: Boolean, argTwo: Int): String
          }
        """).get

      ast.withoutSourceMapper should be(
        Document(
          Vector(
            ObjectTypeDefinition(
              "Hello",
              Vector.empty,
              Vector(
                FieldDefinition(
                  "world",
                  NamedType("String", Some(AstLocation(73, 3, 50))),
                  Vector(
                    InputValueDefinition(
                      "argOne",
                      NamedType("Boolean", Some(AstLocation(50, 3, 27))),
                      None,
                      Vector.empty,
                      None,
                      Vector.empty,
                      Some(AstLocation(42, 3, 19))),
                    InputValueDefinition(
                      "argTwo",
                      NamedType("Int", Some(AstLocation(67, 3, 44))),
                      None,
                      Vector.empty,
                      None,
                      Vector.empty,
                      Some(AstLocation(59, 3, 36)))
                  ),
                  Vector.empty,
                  None,
                  Vector.empty,
                  Some(AstLocation(36, 3, 13))
                )),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(11, 2, 11))
            )),
          Vector.empty,
          Some(AstLocation(11, 2, 11)),
          None
        ))
    }

    "Simple union" in {
      val ast = parseQuery("union Hello = World").get

      ast.withoutSourceMapper should be(
        Document(
          Vector(
            UnionTypeDefinition(
              "Hello",
              Vector(NamedType("World", Some(AstLocation(14, 1, 15)))),
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

    "Union with two types" in {
      val ast = parseQuery("union Hello = Wo | Rld").get

      ast.withoutSourceMapper should be(
        Document(
          Vector(UnionTypeDefinition(
            "Hello",
            Vector(
              NamedType("Wo", Some(AstLocation(14, 1, 15))),
              NamedType("Rld", Some(AstLocation(19, 1, 20)))),
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

    "Scalar" in {
      val ast = parseQuery("scalar Hello").get

      ast.withoutSourceMapper should be(
        Document(
          Vector(
            ScalarTypeDefinition(
              "Hello",
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

    "Simple input object" in {
      val ast = parseQuery("""
          input Hello {
            world: String
          }
        """).get

      ast.withoutSourceMapper should be(
        Document(
          Vector(InputObjectTypeDefinition(
            "Hello",
            Vector(
              InputValueDefinition(
                "world",
                NamedType("String", Some(AstLocation(44, 3, 20))),
                None,
                Vector.empty,
                None,
                Vector.empty,
                Some(AstLocation(37, 3, 13)))),
            Vector.empty,
            None,
            Vector.empty,
            Vector.empty,
            Some(AstLocation(11, 2, 11))
          )),
          Vector.empty,
          Some(AstLocation(11, 2, 11)),
          None
        ))
    }

    "Simple input object with args should fail" in {
      an[SyntaxError] should be thrownBy parseQuery("""
          input Hello {
            world(foo: Int): String
          }
        """).get
    }

    "Allow empty fields and values" in {
      val ast = parseQuery("""
          type Foo @hello
          interface Bar
          input Baz
          enum Test
          union Test @bar
        """).get

      ast.withoutSourceMapper should be(
        Document(
          Vector(
            ObjectTypeDefinition(
              "Foo",
              Vector.empty,
              Vector.empty,
              Vector(
                Directive(
                  "hello",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(20, 2, 20))
                )),
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(11, 2, 11))
            ),
            InterfaceTypeDefinition(
              "Bar",
              Vector.empty,
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(37, 3, 11))
            ),
            InputObjectTypeDefinition(
              "Baz",
              Vector.empty,
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(61, 4, 11))
            ),
            EnumTypeDefinition(
              "Test",
              Vector.empty,
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(81, 5, 11))
            ),
            UnionTypeDefinition(
              "Test",
              Vector.empty,
              Vector(
                Directive(
                  "bar",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(112, 6, 22))
                )),
              None,
              Vector.empty,
              Some(AstLocation(101, 6, 11))
            )
          ),
          Vector.empty,
          Some(AstLocation(11, 2, 11)),
          None
        ))
    }

    "Allow extensions on various types" in {
      val ast = parseQuery("""
          extend type Foo implements Hello & World @hello(ids: [1, 2]) {
           f1: Int
          }

          extend interface Bar @dir {
            "test field"
            f2: [String!]
          }

          extend input Baz {
           inp: Int = 1
          }

          extend enum Color {
            MAGENTA
          }

          extend union Test @bar = More | Types

          extend type EmptyFoo implements Hello1 & World1 @hello1(ids: [1, 2])
          extend interface EmptyBar @dir1
          extend input EmptyBaz @extraDir
          extend enum EmptyColor @beautiful
          extend union EmptyTest @bar
        """).get

      ast.withoutSourceMapper should be(
        Document(
          Vector(
            ObjectTypeExtensionDefinition(
              "Foo",
              Vector(
                NamedType("Hello", Some(AstLocation(38, 2, 38))),
                NamedType("World", Some(AstLocation(46, 2, 46)))),
              Vector(
                FieldDefinition(
                  "f1",
                  NamedType("Int", Some(AstLocation(89, 3, 16))),
                  Vector.empty,
                  Vector.empty,
                  None,
                  Vector.empty,
                  Some(AstLocation(85, 3, 12)))),
              Vector(Directive(
                "hello",
                Vector(Argument(
                  "ids",
                  ListValue(
                    Vector(
                      BigIntValue(1, Vector.empty, Some(AstLocation(65, 2, 65))),
                      BigIntValue(2, Vector.empty, Some(AstLocation(68, 2, 68)))),
                    Vector.empty,
                    Some(AstLocation(64, 2, 64))
                  ),
                  Vector.empty,
                  Some(AstLocation(59, 2, 59))
                )),
                Vector.empty,
                Some(AstLocation(52, 2, 52))
              )),
              Vector.empty,
              Vector.empty,
              Some(AstLocation(11, 2, 11))
            ),
            InterfaceTypeExtensionDefinition(
              "Bar",
              Vector(FieldDefinition(
                "f2",
                ListType(
                  NotNullType(
                    NamedType("String", Some(AstLocation(186, 8, 18))),
                    Some(AstLocation(186, 8, 18))),
                  Some(AstLocation(185, 8, 17))),
                Vector.empty,
                Vector.empty,
                Some(
                  StringValue(
                    "test field",
                    false,
                    None,
                    Vector.empty,
                    Some(AstLocation(156, 7, 13)))),
                Vector.empty,
                Some(AstLocation(181, 8, 13))
              )),
              Vector(
                Directive(
                  "dir",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(137, 6, 32))
                )),
              Vector.empty,
              Vector.empty,
              Some(AstLocation(116, 6, 11))
            ),
            InputObjectTypeExtensionDefinition(
              "Baz",
              Vector(InputValueDefinition(
                "inp",
                NamedType("Int", Some(AstLocation(253, 12, 17))),
                Some(BigIntValue(1, Vector.empty, Some(AstLocation(259, 12, 23)))),
                Vector.empty,
                None,
                Vector.empty,
                Some(AstLocation(248, 12, 12))
              )),
              Vector.empty,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(218, 11, 11))
            ),
            EnumTypeExtensionDefinition(
              "Color",
              Vector(
                EnumValueDefinition(
                  "MAGENTA",
                  Vector.empty,
                  None,
                  Vector.empty,
                  Some(AstLocation(316, 16, 13)))),
              Vector.empty,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(284, 15, 11))
            ),
            UnionTypeExtensionDefinition(
              "Test",
              Vector(
                NamedType("More", Some(AstLocation(372, 19, 36))),
                NamedType("Types", Some(AstLocation(379, 19, 43)))),
              Vector(
                Directive(
                  "bar",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(365, 19, 29))
                )),
              Vector.empty,
              Some(AstLocation(347, 19, 11))
            ),
            ObjectTypeExtensionDefinition(
              "EmptyFoo",
              Vector(
                NamedType("Hello1", Some(AstLocation(428, 21, 43))),
                NamedType("World1", Some(AstLocation(437, 21, 52)))),
              Vector.empty,
              Vector(Directive(
                "hello1",
                Vector(Argument(
                  "ids",
                  ListValue(
                    Vector(
                      BigIntValue(1, Vector.empty, Some(AstLocation(458, 21, 73))),
                      BigIntValue(2, Vector.empty, Some(AstLocation(461, 21, 76)))),
                    Vector.empty,
                    Some(AstLocation(457, 21, 72))
                  ),
                  Vector.empty,
                  Some(AstLocation(452, 21, 67))
                )),
                Vector.empty,
                Some(AstLocation(444, 21, 59))
              )),
              Vector.empty,
              Vector.empty,
              Some(AstLocation(396, 21, 11))
            ),
            InterfaceTypeExtensionDefinition(
              "EmptyBar",
              Vector.empty,
              Vector(
                Directive(
                  "dir1",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(501, 22, 37))
                )),
              Vector.empty,
              Vector.empty,
              Some(AstLocation(475, 22, 11))
            ),
            InputObjectTypeExtensionDefinition(
              "EmptyBaz",
              Vector.empty,
              Vector(
                Directive(
                  "extraDir",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(539, 23, 33))
                )),
              Vector.empty,
              Vector.empty,
              Some(AstLocation(517, 23, 11))
            ),
            EnumTypeExtensionDefinition(
              "EmptyColor",
              Vector.empty,
              Vector(
                Directive(
                  "beautiful",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(582, 24, 34))
                )),
              Vector.empty,
              Vector.empty,
              Some(AstLocation(559, 24, 11))
            ),
            UnionTypeExtensionDefinition(
              "EmptyTest",
              Vector.empty,
              Vector(
                Directive(
                  "bar",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(626, 25, 34))
                )),
              Vector.empty,
              Some(AstLocation(603, 25, 11))
            )
          ),
          Vector.empty,
          Some(AstLocation(11, 2, 11)),
          None
        ))
    }

    "Allow schema with description" in {
      val ast = parseQuery("""
          "the best schema ever"
          schema @dir1 {
            query: Query
          }
        """).get

      ast should be(
        Document(
          Vector(
            SchemaDefinition(
              Vector(
                OperationTypeDefinition(
                  OperationType.Query,
                  NamedType("Query", Some(AstLocation(78, 4, 20))),
                  Vector.empty,
                  Some(AstLocation(71, 4, 13)))),
              Vector(
                Directive(
                  "dir1",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(51, 3, 18))
                )),
              Some(
                StringValue(
                  "the best schema ever",
                  false,
                  None,
                  Vector.empty,
                  Some(AstLocation(11, 2, 11)))),
              Vector.empty,
              Vector.empty,
              Some(AstLocation(44, 3, 11))
            )),
          Vector.empty,
          Some(AstLocation(11, 2, 11)),
          None
        ))
    }

    "Allow extensions on the schema" in {
      val ast = parseQuery("""
          extend schema @dir4

          schema @dir1 {
            query: Query
          }

          extend schema @dir2 @dir3(test: true) {
            mutation: Mutation
          }
        """).get

      ast should be(
        Document(
          Vector(
            SchemaExtensionDefinition(
              Vector.empty,
              Vector(
                Directive(
                  "dir4",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(25, 2, 25))
                )),
              Vector.empty,
              Vector.empty,
              Some(AstLocation(11, 2, 11))
            ),
            SchemaDefinition(
              Vector(
                OperationTypeDefinition(
                  OperationType.Query,
                  NamedType("Query", Some(AstLocation(76, 5, 20))),
                  Vector.empty,
                  Some(AstLocation(69, 5, 13)))),
              Vector(
                Directive(
                  "dir1",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(49, 4, 18))
                )),
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(42, 4, 11))
            ),
            SchemaExtensionDefinition(
              Vector(
                OperationTypeDefinition(
                  OperationType.Mutation,
                  NamedType("Mutation", Some(AstLocation(167, 9, 23))),
                  Vector.empty,
                  Some(AstLocation(157, 9, 13)))),
              Vector(
                Directive(
                  "dir2",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(119, 8, 25))
                ),
                Directive(
                  "dir3",
                  Vector(
                    Argument(
                      "test",
                      BooleanValue(true, Vector.empty, Some(AstLocation(137, 8, 43))),
                      Vector.empty,
                      Some(AstLocation(131, 8, 37))
                    )),
                  Vector.empty,
                  Some(AstLocation(125, 8, 31))
                )
              ),
              Vector.empty,
              Vector.empty,
              Some(AstLocation(105, 8, 11))
            )
          ),
          Vector.empty,
          Some(AstLocation(11, 2, 11)),
          None
        ))
    }

    "properly handle new lines in the block-string" in {
      val q = "\"\"\""
      val ast = parseQuery(s"""type Hello {
           |  $q
           |  some description
           |  $q
           |  field1: Bar
           |}""".stripMargin).get

      ast.withoutSourceMapper should be(
        Document(
          Vector(
            ObjectTypeDefinition(
              "Hello",
              Vector.empty,
              Vector(FieldDefinition(
                "field1",
                NamedType("Bar", Some(AstLocation(54, 5, 11))),
                Vector.empty,
                Vector.empty,
                Some(
                  StringValue(
                    "some description",
                    true,
                    Some("\n  some description\n  "),
                    Vector.empty,
                    Some(AstLocation(15, 2, 3)))),
                Vector.empty,
                Some(AstLocation(46, 5, 3))
              )),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(0, 1, 1))
            )),
          Vector.empty,
          Some(AstLocation(0, 1, 1)),
          None
        ))
    }

    "Allow interface implementing interface" in {

      val ast = parseQuery("""
      |interface Node {
      |  id: ID!
      |}
      |
      |interface Resource implements Node {
      |  id: ID!
      |  url: String
      |}
      """.stripMargin).get

      ast.withoutSourceMapper should be(
        Document(
          Vector(
            InterfaceTypeDefinition(
              "Node",
              Vector(
                FieldDefinition(
                  "id",
                  NotNullType(
                    NamedType("ID", Some(AstLocation(24, 3, 7))),
                    Some(AstLocation(24, 3, 7))
                  ),
                  Vector.empty,
                  Vector.empty,
                  None,
                  Vector.empty,
                  Some(AstLocation(20, 3, 3))
                )
              ),
              Vector.empty,
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(1, 2, 1))
            ),
            InterfaceTypeDefinition(
              "Resource",
              Vector(
                FieldDefinition(
                  "id",
                  NotNullType(
                    NamedType("ID", Some(AstLocation(74, 7, 7))),
                    Some(AstLocation(74, 7, 7))
                  ),
                  Vector.empty,
                  Vector.empty,
                  None,
                  Vector.empty,
                  Some(AstLocation(70, 7, 3))
                ),
                FieldDefinition(
                  "url",
                  NamedType("String", Some(AstLocation(85, 8, 8))),
                  Vector.empty,
                  Vector.empty,
                  None,
                  Vector.empty,
                  Some(AstLocation(80, 8, 3))
                )
              ),
              Vector(
                NamedType("Node", Some(AstLocation(61, 6, 31)))
              ),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(31, 6, 1))
            )
          ),
          Vector.empty,
          Some(AstLocation(1, 2, 1)),
          None
        )
      )

    }

    "Allow extensions on interface implementing interface" in {

      val ast = parseQuery("""
      |interface Foo {
      |  foo: String
      |}
      |
      |interface Bar {
      |  bar: Int
      |}
      |
      |interface Baz implements Foo {
      |  foo: String
      |}
      |
      |extend interface Baz implements Bar {
      |  baz: Boolean
      |  bar: Int
      |  foo: String
      |}
      """.stripMargin).get

      ast.withoutSourceMapper should be(
        Document(
          Vector(
            InterfaceTypeDefinition(
              "Foo",
              Vector(
                FieldDefinition(
                  "foo",
                  NamedType("String", Some(AstLocation(24, 3, 8))),
                  Vector.empty,
                  Vector.empty,
                  None,
                  Vector.empty,
                  Some(AstLocation(19, 3, 3)))),
              Vector.empty,
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(1, 2, 1))
            ),
            InterfaceTypeDefinition(
              "Bar",
              Vector(
                FieldDefinition(
                  "bar",
                  NamedType("Int", Some(AstLocation(57, 7, 8))),
                  Vector.empty,
                  Vector.empty,
                  None,
                  Vector.empty,
                  Some(AstLocation(52, 7, 3)))),
              Vector.empty,
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(34, 6, 1))
            ),
            InterfaceTypeDefinition(
              "Baz",
              Vector(
                FieldDefinition(
                  "foo",
                  NamedType("String", Some(AstLocation(102, 11, 8))),
                  Vector.empty,
                  Vector.empty,
                  None,
                  Vector.empty,
                  Some(AstLocation(97, 11, 3)))),
              Vector(NamedType("Foo", Some(AstLocation(89, 10, 26)))),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(64, 10, 1))
            ),
            InterfaceTypeExtensionDefinition(
              "Baz",
              Vector(NamedType("Bar", Some(AstLocation(144, 14, 33)))),
              Vector(
                FieldDefinition(
                  "baz",
                  NamedType("Boolean", Some(AstLocation(157, 15, 8))),
                  Vector.empty,
                  Vector.empty,
                  None,
                  Vector.empty,
                  Some(AstLocation(152, 15, 3))),
                FieldDefinition(
                  "bar",
                  NamedType("Int", Some(AstLocation(172, 16, 8))),
                  Vector.empty,
                  Vector.empty,
                  None,
                  Vector.empty,
                  Some(AstLocation(167, 16, 3))),
                FieldDefinition(
                  "foo",
                  NamedType("String", Some(AstLocation(183, 17, 8))),
                  Vector.empty,
                  Vector.empty,
                  None,
                  Vector.empty,
                  Some(AstLocation(178, 17, 3)))
              ),
              Vector.empty,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(112, 14, 1))
            )
          ),
          Vector.empty,
          Some(AstLocation(1, 2, 1)),
          None
        )
      )

    }

  }
}
