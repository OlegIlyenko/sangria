package sangria.marshalling

import sangria.schema._
import sangria.util.SimpleGraphQlSupport._
import spray.json._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import sangria.util.tag.@@ // Scala 3 issue workaround
import sangria.marshalling.FromInput.InputObjectResult

class FromInputSpec extends AnyWordSpec with Matchers {
  case class Comment(author: String, text: Option[String])
  case class Article(
      title: String,
      text: Option[String],
      tags: Option[Vector[String]],
      comments: Vector[Option[Comment]])

  object MyJsonProtocol extends DefaultJsonProtocol {
    implicit val commentFormat: JsonFormat[Comment] = jsonFormat2(Comment.apply)
    implicit val articleFormat: JsonFormat[Article] = jsonFormat4(Article.apply)
  }

  import sangria.marshalling.sprayJson.sprayJsonWriterToInput

  val CommentType = InputObjectType[Comment](
    "Comment",
    List(
      InputField("author", OptionInputType(StringType), defaultValue = "anonymous"),
      InputField("text", OptionInputType(StringType))
    ))

  val ArticleType = InputObjectType[Article](
    "Article",
    List(
      InputField("title", StringType),
      InputField("text", OptionInputType(StringType)),
      InputField("tags", OptionInputType(ListInputType(StringType))),
      InputField("comments", ListInputType(OptionInputType(CommentType)))
    )
  )

  def manualSprayJsonSchema = {
    import sangria.marshalling.sprayJson.sprayJsonFromInput
    import MyJsonProtocol._

    val TestType = ObjectType(
      "TestType", {
        val Comment1Type = InputObjectType[JsValue](
          "Comment",
          List(
            InputField("author", OptionInputType(StringType), defaultValue = "anonymous"),
            InputField("text", OptionInputType(StringType))
          ))

        val Article1Type = InputObjectType[JsValue](
          "Article",
          List(
            InputField("title", StringType),
            InputField("text", OptionInputType(StringType)),
            InputField("tags", OptionInputType(ListInputType(StringType))),
            InputField("comments", ListInputType(OptionInputType(Comment1Type)))
          )
        )

        fields[Unit, Unit] {
          val arg = Argument(
            "articles",
            OptionInputType(ListInputType(OptionInputType(Article1Type))),
            Vector(
              Some(
                Article(
                  "def1",
                  None,
                  Some(Vector("c", "d")),
                  Vector(Some(Comment("c1", None)), None))),
              None,
              Some(Article("def2", Some("some text"), None, Vector.empty))
            )
          )

          Field(
            "optListOpt",
            OptionType(StringType),
            arguments = arg :: Nil,
            resolve = ctx => {
              val value: Seq[Option[JsValue]] = ctx.arg(arg)

              "" + value.map(_.map(_.compactPrint))
            })
        }
      }
    )

    Schema(TestType)
  }

  def automaticWithJsonFormatSchema = {
    import MyJsonProtocol._
    import sangria.marshalling.sprayJson.sprayJsonReaderFromInput

    val TestType = ObjectType(
      "TestType", {
        val Comment1Type = InputObjectType[Comment](
          "Comment",
          List(
            InputField("author", OptionInputType(StringType), defaultValue = "anonymous"),
            InputField("text", OptionInputType(StringType))
          ))

        val Article1Type = InputObjectType[Article](
          "Article",
          List(
            InputField("title", StringType),
            InputField("text", OptionInputType(StringType)),
            InputField("tags", OptionInputType(ListInputType(StringType))),
            InputField("comments", ListInputType(OptionInputType(Comment1Type)))
          )
        )

        fields[Unit, Unit](
          {
            val arg = Argument("article", Article1Type)

            Field(
              "nn",
              OptionType(StringType),
              arguments = arg :: Nil,
              resolve = ctx => {
                val value: Article = ctx.arg(arg)

                "" + value
              })
          }, {
            val arg =
              Argument[Option[Seq[Option[Article @@ InputObjectResult]]], Seq[Option[Article]]](
                "articles",
                OptionInputType(ListInputType(OptionInputType(Article1Type))),
                Vector(
                  Some(
                    Article(
                      "def1",
                      None,
                      Some(Vector("c", "d")),
                      Vector(Some(Comment("c1", None)), None))),
                  None,
                  Some(Article("def2", Some("some text"), None, Vector.empty))
                )
              )

            Field(
              "optListOpt",
              OptionType(StringType),
              arguments = arg :: Nil,
              resolve = ctx => {
                val value: Seq[Option[Article]] = ctx.arg(arg)

                "" + value
              })
          }
        )
      }
    )

    Schema(TestType)
  }

  def manualCoercedScalaSchema = {
    import MyJsonProtocol._

    implicit val manual = new FromInput[Article] {
      val marshaller = CoercedScalaResultMarshaller.default
      def fromResult(node: marshaller.Node) = {
        val ad = node.asInstanceOf[Map[String, Any]]

        def readComments(data: Seq[Option[Map[String, Any]]]) =
          data.toVector.map(_.map { cd =>
            Comment(
              author = cd("author").asInstanceOf[Option[String]].getOrElse("manual default"),
              text = cd.get("text").flatMap(_.asInstanceOf[Option[String]]))
          })

        Article(
          title = ad("title").asInstanceOf[String],
          text = ad.get("text").flatMap(_.asInstanceOf[Option[String]]),
          tags = ad.get("tags").flatMap(_.asInstanceOf[Option[Seq[String]]]).map(_.toVector),
          comments = readComments(ad("comments").asInstanceOf[Seq[Option[Map[String, Any]]]])
        )
      }
    }

    val TestType = ObjectType(
      "TestType",
      fields[Unit, Unit](
        {
          val arg = Argument("article", ArticleType)

          Field(
            "nn",
            OptionType(StringType),
            arguments = arg :: Nil,
            resolve = ctx => {
              val value: Article = ctx.arg(arg)

              "" + value
            })
        }, {
          val arg = Argument("article", OptionInputType(ArticleType))

          Field(
            "opt",
            OptionType(StringType),
            arguments = arg :: Nil,
            resolve = ctx => {
              val value: Option[Article] = ctx.arg(arg)

              "" + value
            })
        }, {
          val arg = Argument(
            "article",
            OptionInputType(ArticleType),
            Article(
              "def",
              None,
              None,
              Vector(Some(Comment("aaa", None)), Some(Comment("bbb", Some("ccc"))), None)))

          Field(
            "optDef",
            OptionType(StringType),
            arguments = arg :: Nil,
            resolve = ctx => {
              val value: Article = ctx.arg(arg)

              "" + value
            })
        }, {
          val arg = Argument("articles", OptionInputType(ListInputType(ArticleType)))

          Field(
            "optList",
            OptionType(StringType),
            arguments = arg :: Nil,
            resolve = ctx => {
              val value: Option[Seq[Article]] = ctx.arg(arg)

              "" + value
            })
        }, {
          val arg = Argument(
            "articles",
            OptionInputType(ListInputType(ArticleType)),
            Vector(
              Article(
                "def1",
                None,
                Some(Vector("c", "d")),
                Vector(Some(Comment("c1", None)), None)),
              Article("def2", Some("some text"), None, Vector.empty)
            )
          )

          Field(
            "optListDef",
            OptionType(StringType),
            arguments = arg :: Nil,
            resolve = ctx => {
              val value: Seq[Article] = ctx.arg(arg)

              "" + value
            })
        }, {
          val arg =
            Argument("articles", OptionInputType(ListInputType(OptionInputType(ArticleType))))

          Field(
            "optListOpt",
            OptionType(StringType),
            arguments = arg :: Nil,
            resolve = ctx => {
              val value: Option[Seq[Option[Article]]] = ctx.arg(arg)

              "" + value
            })
        }
      )
    )

    Schema(TestType)
  }

  "FromInput" should {
    "deserialize manually with coerced scala result marshaller (single not-null value)" in check(
      manualCoercedScalaSchema,
      (),
      """
        {
          nn(article: {
            title: "First!"
            text: null
            comments: [null, {text: "Hello wold"}]
          })
        }
      """,
      Map(
        "data" -> Map(
          "nn" -> Article(
            "First!",
            None,
            None,
            Vector(None, Some(Comment("anonymous", Some("Hello wold"))))).toString))
    )

    "deserialize manually with coerced scala result marshaller (single optional value)" in check(
      manualCoercedScalaSchema,
      (),
      """
        {
          o1: opt
          o2: opt(article: null)
          o3: opt(article: {title: "foo", text: "bar", tags: null, comments: []})
        }
      """,
      Map(
        "data" -> Map(
          "o1" -> None.toString,
          "o2" -> None.toString,
          "o3" -> Some(Article("foo", Some("bar"), None, Vector.empty)).toString))
    )

    "deserialize manually with coerced scala result marshaller (single optional value with default)" in check(
      manualCoercedScalaSchema,
      (),
      """
        {
          od1: optDef(article: null)
          od2: optDef(article: {title: "foo", text: "bar", tags: null, comments: []})
        }
      """,
      Map(
        "data" -> Map(
          "od1" -> Article(
            "def",
            None,
            None,
            Vector(Some(Comment("aaa", None)), Some(Comment("bbb", Some("ccc"))), None)).toString,
          "od2" -> Article("foo", Some("bar"), None, Vector.empty).toString
        ))
    )

    "deserialize manually with coerced scala result marshaller (optional list with not-null values)" in check(
      manualCoercedScalaSchema,
      (),
      """
        query Test($var1: Article!, $var2: [Article!]) {
          ol1: optList(articles: null)
          ol2: optList(articles: [{title: "first", comments: [null]}, {title: "second", comments: [null, null]}])
          ol3: optList(articles: [$var1])
          ol4: optList(articles: $var2)
        }
      """,
      Map(
        "data" -> Map(
          "ol1" -> None.toString,
          "ol2" -> Some(
            Vector(
              Article("first", None, None, Vector(None)),
              Article("second", None, None, Vector(None, None)))).toString,
          "ol3" -> Some(
            Vector(
              Article(
                "foo",
                Some("bar"),
                Some(Vector("a", "b")),
                Vector(
                  None,
                  Some(Comment("anonymous", None)),
                  Some(Comment("anonymous", Some("comment3"))))))).toString,
          "ol4" -> Some(
            Vector(
              Article("bar", None, None, Vector(None)),
              Article("baz", None, None, Vector.empty))).toString
        )),
      """
        {
          "var1": {
            "title": "foo",
            "text": "bar",
            "tags": ["a", "b"],
            "comments": [
              null,
              {},
              {"text": "comment3"}
            ]
          },
          "var2": [
            {"title": "bar", "text": null, "tags": null, "comments": [null]},
            {"title": "baz", "comments": []}
          ]
        }
      """.parseJson
    )

    "deserialize manually with coerced scala result marshaller (optional list with not-null values and default)" in check(
      manualCoercedScalaSchema,
      (),
      """
        {
          old1: optListDef(articles: null)
          old2: optListDef(articles: [{title: "first", comments: [null]}, {title: "second", comments: [null, null]}])
        }
      """,
      Map(
        "data" -> Map(
          "old1" -> Vector(
            Article("def1", None, Some(Vector("c", "d")), Vector(Some(Comment("c1", None)), None)),
            Article("def2", Some("some text"), None, Vector.empty)).toString,
          "old2" -> Vector(
            Article("first", None, None, Vector(None)),
            Article("second", None, None, Vector(None, None))).toString
        ))
    )

    "deserialize manually with coerced scala result marshaller (optional list with optional values)" in check(
      manualCoercedScalaSchema,
      (),
      """
        query Test($var1: Article!, $var2: [Article]) {
          olo1: optListOpt(articles: null)
          olo2: optListOpt(articles: [{title: "first", comments: [null]}, null, {title: "second", comments: [null, null]}])
          olo3: optListOpt(articles: [$var1, null])
          olo4: optListOpt(articles: $var2)
        }
      """,
      Map(
        "data" -> Map(
          "olo1" -> None.toString,
          "olo2" -> Some(
            Vector(
              Some(Article("first", None, None, Vector(None))),
              None,
              Some(Article("second", None, None, Vector(None, None))))).toString,
          "olo3" -> Some(Vector(
            Some(
              Article(
                "foo",
                Some("bar"),
                Some(Vector("a", "b")),
                Vector(
                  None,
                  Some(Comment("anonymous", None)),
                  Some(Comment("anonymous", Some("comment3")))))),
            None
          )).toString,
          "olo4" -> Some(
            Vector(
              Some(Article("bar", None, None, Vector(None))),
              None,
              Some(Article("baz", None, None, Vector.empty)))).toString
        )),
      """
        {
          "var1": {
            "title": "foo",
            "text": "bar",
            "tags": ["a", "b"],
            "comments": [
              null,
              {},
              {"text": "comment3"}
            ]
          },
          "var2": [
            {"title": "bar", "text": null, "tags": null, "comments": [null]},
            null,
            {"title": "baz", "comments": []}
          ]
        }
      """.parseJson
    )

    "deserialize manually with spray json result marshaller (optional list with optional values and default values)" in check(
      manualSprayJsonSchema,
      (),
      """
        query Test($var1: Article!, $var2: [Article]) {
          olo1: optListOpt(articles: null)
          olo2: optListOpt(articles: [{title: "first", comments: [null]}, null, {title: "second", comments: [null, null]}])
          olo3: optListOpt(articles: [$var1, null])
          olo4: optListOpt(articles: $var2)
        }
      """,
      Map(
        "data" -> Map(
          "olo1" -> Vector(
            Some("""{"title":"def1","tags":["c","d"],"comments":[{"author":"c1"},null]}"""),
            None,
            Some("""{"title":"def2","text":"some text","comments":[]}""")).toString,
          "olo2" -> Vector(
            Some("""{"title":"first","comments":[null]}"""),
            None,
            Some("""{"title":"second","comments":[null,null]}""")).toString,
          "olo3" -> Vector(
            Some("""{"title":"foo","text":"bar","tags":["a","b"],"comments":[null,{"author":"anonymous"},{"author":"anonymous","text":"comment3"}]}"""),
            None).toString,
          "olo4" -> Vector(
            Some("""{"title":"bar","text":null,"tags":null,"comments":[null]}"""),
            None,
            Some("""{"title":"baz","comments":[]}""")).toString
        )),
      """
        {
          "var1": {
            "title": "foo",
            "text": "bar",
            "tags": ["a", "b"],
            "comments": [
              null,
              {},
              {"text": "comment3"}
            ]
          },
          "var2": [
            {"title": "bar", "text": null, "tags": null, "comments": [null]},
            null,
            {"title": "baz", "comments": []}
          ]
        }
      """.parseJson
    )

    "deserialize automatically with spray-json JsonFormat (single not-null value)" in check(
      automaticWithJsonFormatSchema,
      (),
      """
        {
          nn(article: {
            title: "First!"
            text: null
            comments: [null, {text: "Hello wold"}]
          })
        }
      """,
      Map(
        "data" -> Map(
          "nn" -> Article(
            "First!",
            None,
            None,
            Vector(None, Some(Comment("anonymous", Some("Hello wold"))))).toString))
    )

    "deserialize automatically with spray-json JsonFormat (optional list with optional values and default)" in check(
      automaticWithJsonFormatSchema,
      (),
      """
        query Test($var1: Article!, $var2: [Article]) {
          olo1: optListOpt(articles: null)
          olo2: optListOpt(articles: [{title: "first", comments: [null]}, null, {title: "second", comments: [null, null]}])
          olo3: optListOpt(articles: [$var1, null])
          olo4: optListOpt(articles: $var2)
        }
      """,
      Map(
        "data" -> Map(
          "olo1" -> Vector(
            Some(
              Article(
                "def1",
                None,
                Some(Vector("c", "d")),
                Vector(Some(Comment("c1", None)), None))),
            None,
            Some(Article("def2", Some("some text"), None, Vector.empty))
          ).toString,
          "olo2" -> Vector(
            Some(Article("first", None, None, Vector(None))),
            None,
            Some(Article("second", None, None, Vector(None, None)))).toString,
          "olo3" -> Vector(
            Some(
              Article(
                "foo",
                Some("bar"),
                Some(Vector("a", "b")),
                Vector(
                  None,
                  Some(Comment("anonymous", None)),
                  Some(Comment("anonymous", Some("comment3")))))),
            None
          ).toString,
          "olo4" -> Vector(
            Some(Article("bar", None, None, Vector(None))),
            None,
            Some(Article("baz", None, None, Vector.empty))).toString
        )),
      """
        {
          "var1": {
            "title": "foo",
            "text": "bar",
            "tags": ["a", "b"],
            "comments": [
              null,
              {},
              {"text": "comment3"}
            ]
          },
          "var2": [
            {"title": "bar", "text": null, "tags": null, "comments": [null]},
            null,
            {"title": "baz", "comments": []}
          ]
        }
      """.parseJson
    )
  }
}
