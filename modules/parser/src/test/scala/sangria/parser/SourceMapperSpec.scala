package sangria.parser

import org.scalatest.verbs.MustVerb
import org.scalatest.wordspec.AnyWordSpec

class SourceMapperSpec extends AnyWordSpec with MustVerb {
  "A reference to the former SourceMapper name" must {
    "compile" in
      assertCompiles {
        """import org.parboiled2.ParserInput
          |import sangria.ast.{DefaultSourceMapper, SourceMapperInput}
          |
          |val input = ParserInput("Some input.")
          |val mapper: sangria.parser.SourceMapper =
          |  new DefaultSourceMapper("", new SourceMapperInput {
          |    override def source: String = input.sliceString(0, input.length)
          |    override def getLine(line: Int): String = input.getLine(line)
          |  })
          |""".stripMargin
      }
  }
}
