package tutorial.webapp

import tutorial.webapp.Lexer._

import scala.language.postfixOps
import scala.util.parsing.combinator.RegexParsers

class SchemaParser extends RegexParsers {
  private def sectionParsers(): Parser[SectionToken] = {
    val section_parser: Parser[SectionToken] = SectionLexer().asInstanceOf[Parser[SectionToken]]
    val textinput_parser: Parser[TextInputToken] = TextInputTokenLexer().asInstanceOf[Parser[TextInputToken]]
    val select_parser: Parser[SelectToken] = SelectLexer().asInstanceOf[Parser[SelectToken]]
    section_parser | textinput_parser | select_parser
  }

  def getTokens(grammar: Array[String]): List[Token] = {
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
    parse(getParser(), line) match {
      case NoSuccess(msg, next) =>
        println(s"Could not parse line = [$line]. error=${msg}")
        None

      case Success(resultList, next) =>
        println(s"Successfully parsed line = [$line].")
        resultList.headOption
    }
  }

  private def getParser(): Parser[List[Token]] = {
    val exit: Parser[EXIT.type] = ExitLexer().asInstanceOf[Parser[EXIT.type]]
    phrase(rep1(exit | sectionParsers()))
  }
}