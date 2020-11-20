package tutorial.webapp

import tutorial.webapp.Lexer._

import scala.language.postfixOps
import scala.util.control.NonFatal
import scala.util.parsing.combinator.RegexParsers

class SchemaParser extends RegexParsers {
  private def sectionParsers(): Parser[SectionTokens] = {
    val section_parser: Parser[SectionToken] = SectionLexer().asInstanceOf[Parser[SectionToken]]
    val heading_parser: Parser[HeadingToken] = HeadingLexer().asInstanceOf[Parser[HeadingToken]]
    val textinput_parser: Parser[TextInputToken] = TextInputTokenLexer().asInstanceOf[Parser[TextInputToken]]
    (section_parser | heading_parser | textinput_parser)
  }

  def getTokens(): List[Token] = {
    val grammar: Array[String] =
      """# this is a comment
        |DEFINE SECTION ContentBranding
        |  HEADING title="Content branding details"
        |  TEXTINPUT name="Name" id="textInput1" value="Type here" required=true
        |  TEXTINPUT name="Description" id="textInput2" value="Type here"
        |;
        |""".stripMargin.split("\\n")

    (for {
      code <- grammar
      line = code.trim
      if line.nonEmpty && line.head != '#'
    } yield {
      println(s"parsing line = $code")
      get(line)
    }).toList.flatten
  }

  private def get(line: String): Option[Token] = {
    try {
      parse(getParser(), line) match {
        case NoSuccess(msg, next) =>
          println(s"Could not parse line = [$line]. error=${msg}")
          None

        case Success(resultList, next) =>
          println(s"Successfully parsed line = [$line].")
          resultList.headOption
      }

    } catch {
      case NonFatal(e) =>
        //e.printStackTrace()
        None
    }
  }

  private def getParser(): Parser[List[Token]] = {
    val exit: Parser[EXIT.type] = ExitLexer().asInstanceOf[Parser[EXIT.type]]
    phrase(rep1(exit | sectionParsers()))
  }
}