package sangria.execution

import java.util.UUID

import eu.timepit.refined._
import eu.timepit.refined.numeric._
import eu.timepit.refined.api.Refined
import sangria.util.{FutureResultSupport, Pos}
import sangria.schema._
import sangria.macros._
import sangria.marshalling.ScalaInput.scalaInput
import sangria.validation.ValueCoercionViolation
import sangria.util.SimpleGraphQlSupport._

import scala.concurrent.ExecutionContext.Implicits.global
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import sangria.util.tag.@@ // Scala 3 issue workaround
import sangria.marshalling.FromInput.CoercedScalaResult

class ScalarAliasSpec extends AnyWordSpec with Matchers with FutureResultSupport {
  import ScalarAliasSpec._

  case class User(id: UserId, id2: Option[UserId], name: String, num: Int Refined Positive)

  case class RefineViolation(error: String) extends ValueCoercionViolation(error)

  implicit val UserIdType: ScalarAlias[UserId, String] =
    ScalarAlias[UserId, String](StringType, _.id, id => Right(UserId(id)))

  implicit val PositiveIntType: ScalarAlias[Int Refined Positive, Int] =
    ScalarAlias[Int Refined Positive, Int](
      IntType,
      _.value,
      i => refineV[Positive](i).left.map(RefineViolation))

  case object IDViolation extends ValueCoercionViolation("Invalid ID")

  val UUIDType = ScalarAlias[UUID, String](
    StringType,
    toScalar = _.toString,
    fromScalar = idString =>
      try Right(UUID.fromString(idString))
      catch {
        case _: IllegalArgumentException => Left(IDViolation)
      })

  val UserType = ObjectType(
    "User",
    fields[Unit, User](
      Field("id", UserIdType, resolve = _.value.id),
      Field("id2", OptionType(UserIdType), resolve = _.value.id2),
      Field("name", StringType, resolve = _.value.name),
      Field("num", PositiveIntType, resolve = _.value.num)
    )
  )

  val ComplexInputType = InputObjectType(
    "Complex",
    List(
      InputField("userId", OptionInputType(UserIdType), defaultValue = "5678"),
      InputField("userNum", OptionInputType(PositiveIntType))))

  val UserIdArg = Argument("id", UserIdType)
  val NumArg = Argument("n", PositiveIntType)
  val ComplexArg = Argument("c", ComplexInputType)
  val UUIDArg = Argument("id", UUIDType)
  val ComplexUUIDArg =
    Argument("cid", OptionInputType(ListInputType(ListInputType(OptionInputType(UUIDType)))))

  "ScalarAlias" should {
    "represent value class as scalar type" in {
      val schema =
        Schema(
          ObjectType(
            "Query",
            fields[Unit, Unit](
              Field(
                "user",
                UserType,
                arguments = UserIdArg :: NumArg :: ComplexArg :: Nil,
                resolve = _.withArgs(UserIdArg, NumArg, ComplexArg)((userId, num, complex) =>
                  User(userId, complex("userId").asInstanceOf[Option[UserId]], "generated", num))
              ),
              Field(
                "idTest",
                UUIDType,
                arguments = UUIDArg :: Nil,
                resolve = c => {
                  val uuid: UUID = c.arg(UUIDArg)
                  uuid
                })
            )
          ))

      val query =
        graphql"""
          {
            user(id: "1234", n: 42, c: {userNum: 500}) {
              id
              id2
              name
              num
            }

            __type(name: "User") {
              name
              fields {
                name

                type {
                  kind
                  ofType {
                    kind
                    name
                  }
                }
              }
            }
          }
        """

      Executor.execute(schema, query).await should be(
        Map(
          "data" -> Map(
            "user" -> Map("id" -> "1234", "id2" -> "5678", "name" -> "generated", "num" -> 42),
            "__type" -> Map(
              "name" -> "User",
              "fields" -> Vector(
                Map(
                  "name" -> "id",
                  "type" -> Map(
                    "kind" -> "NON_NULL",
                    "ofType" -> Map("kind" -> "SCALAR", "name" -> "String"))),
                Map("name" -> "id2", "type" -> Map("kind" -> "SCALAR", "ofType" -> null)),
                Map(
                  "name" -> "name",
                  "type" -> Map(
                    "kind" -> "NON_NULL",
                    "ofType" -> Map("kind" -> "SCALAR", "name" -> "String"))),
                Map(
                  "name" -> "num",
                  "type" -> Map(
                    "kind" -> "NON_NULL",
                    "ofType" -> Map("kind" -> "SCALAR", "name" -> "Int")))
              )
            )
          )))
    }

    "represent correct transforms UUID values coming from variables (also after AST-based schema extension)" in {
      val TestInp =
        InputObjectType("TestInp", List(InputField("id", UUIDType), InputField("id1", UUIDType)))

      val InpArg = Argument("inp", TestInp)

      val schema =
        Schema(
          ObjectType(
            "Query",
            fields[Unit, Unit](
              Field(
                "idTest",
                UUIDType,
                arguments = UUIDArg :: Nil,
                resolve = c => {
                  val uuid: UUID = c.arg(UUIDArg)
                  uuid
                }),
              Field(
                "cidTest",
                OptionType(ListType(ListType(OptionType(UUIDType)))),
                arguments = ComplexUUIDArg :: Nil,
                resolve = c => {
                  val cuuid: Option[Seq[Seq[Option[UUID]]]] = c.arg(ComplexUUIDArg)
                  cuuid
                }
              ),
              Field(
                "inpTest",
                StringType,
                arguments = InpArg :: Nil,
                resolve = c => {
                  val inp = c.arg(InpArg)

                  val id: UUID = inp("id").asInstanceOf[UUID]
                  val id1: UUID = inp("id1").asInstanceOf[UUID]

                  s"${id.toString}/${id1.toString}"
                }
              )
            )
          ))

      val query =
        gql"""
          query Test($$id: String!, $$inp: TestInp!, $$id1: String = "9454db18-2ce5-11e7-93ae-92361f002671", $$ids: [[String]!], $$ids1: [[String]!] = [["ad6a2dd9-ccd0-44dc-86d2-80cf945cb16e", "6297bd9e-2647-4770-a791-5c3f44bc56ee"], [null, "a9525f38-380b-4226-a362-471ece962f06"]]){
            i1: idTest(id: $$id)
            i2: idTest(id: $$id1)
            inp1: inpTest(inp: {id: $$id, id1: $$id1})
            inp2: inpTest(inp: $$inp)

            ci1: cidTest(cid: [["ad6a2dd9-ccd0-44dc-86d2-80cf945cb16e", "6297bd9e-2647-4770-a791-5c3f44bc56ee"], [null, "a9525f38-380b-4226-a362-471ece962f06"]])
            ci2: cidTest(cid: [[$$id, null, $$id1], ["dd96051e-21c5-468b-ad93-43241acd9540"]])
            ci3: cidTest(cid: $$ids)
            ci4: cidTest(cid: $$ids1)
          }
        """

      val vars = Map(
        "id" -> "f28efae0-8808-4514-b356-02808d4e936c",
        "inp" -> Map(
          "id" -> "9454d352-2ce5-11e7-93ae-92361f002671",
          "id1" -> "9454d5e6-2ce5-11e7-93ae-92361f002671"),
        "ids" -> Seq(
          Seq("9153a6c1-fb4b-4d69-b9aa-ee95765cf093", null, "1a1e42c3-b79b-4dbb-ad89-4ee223ffb6be"),
          Seq(null, "4e4548b0-87db-49b6-a764-2d84c2322fb7"))
      )

      val schema1 =
        schema.extend(gql"""
          extend type Query {
            foo: Int
          }
        """)

      Seq(schema, schema1).foreach { s =>
        Executor.execute(s, query, variables = scalaInput(vars)).await should be(
          Map(
            "data" -> Map(
              "i1" -> "f28efae0-8808-4514-b356-02808d4e936c",
              "i2" -> "9454db18-2ce5-11e7-93ae-92361f002671",
              "inp1" -> "f28efae0-8808-4514-b356-02808d4e936c/9454db18-2ce5-11e7-93ae-92361f002671",
              "inp2" -> "9454d352-2ce5-11e7-93ae-92361f002671/9454d5e6-2ce5-11e7-93ae-92361f002671",
              "ci1" -> Vector(
                Vector(
                  "ad6a2dd9-ccd0-44dc-86d2-80cf945cb16e",
                  "6297bd9e-2647-4770-a791-5c3f44bc56ee"),
                Vector(null, "a9525f38-380b-4226-a362-471ece962f06")),
              "ci2" -> Vector(
                Vector(
                  "f28efae0-8808-4514-b356-02808d4e936c",
                  null,
                  "9454db18-2ce5-11e7-93ae-92361f002671"),
                Vector("dd96051e-21c5-468b-ad93-43241acd9540")),
              "ci3" -> Vector(
                Vector(
                  "9153a6c1-fb4b-4d69-b9aa-ee95765cf093",
                  null,
                  "1a1e42c3-b79b-4dbb-ad89-4ee223ffb6be"),
                Vector(null, "4e4548b0-87db-49b6-a764-2d84c2322fb7")),
              "ci4" -> Vector(
                Vector(
                  "ad6a2dd9-ccd0-44dc-86d2-80cf945cb16e",
                  "6297bd9e-2647-4770-a791-5c3f44bc56ee"),
                Vector(null, "a9525f38-380b-4226-a362-471ece962f06"))
            )))
      }
    }

    "coerces input types correctly" in {
      val schema = Schema(
        ObjectType(
          "Query",
          fields[Unit, Unit](
            Field(
              "user",
              UserType,
              arguments = UserIdArg :: NumArg :: ComplexArg :: Nil,
              resolve = _.withArgs(UserIdArg, NumArg, ComplexArg)((userId, num, complex) =>
                User(
                  userId,
                  complex("userId").asInstanceOf[Option[UserId]],
                  "generated " + complex,
                  num))
            )
          )
        ))

      val query =
        graphql"""
          {
            user(id: "1234", n: -123, c: {userId: 1, userNum: -5}) {
              id
              name
            }
          }
        """

      val error = intercept[ValidationError](Executor.execute(schema, query).await)

      assertViolations(
        error.violations,
        "Expected type 'Int!', found '-123'. Predicate failed: (-123 > 0)." -> Seq(Pos(3, 33)),
        "Expected type 'String', found '1'. String value expected" -> Seq(Pos(3, 51)),
        "Expected type 'Int', found '-5'. Predicate failed: (-5 > 0)." -> Seq(Pos(3, 63))
      )
    }
  }
}

object ScalarAliasSpec {
  case class UserId(id: String) extends AnyVal
}
