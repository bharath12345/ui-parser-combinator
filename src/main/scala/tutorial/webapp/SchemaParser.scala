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

  private def logicParsers(): Parser[LogicToken] = {
    val ifEqualsLexer: Parser[IfEqualsToken] = IfEqualsLexer().asInstanceOf[Parser[IfEqualsToken]]
    val ifMatchesLexer: Parser[IfMatchesToken] = IfMatchesLexer().asInstanceOf[Parser[IfMatchesToken]]
    val ifContainsLexer: Parser[IfContainsToken] = IfContainsLexer().asInstanceOf[Parser[IfContainsToken]]
    val if_parsers = (ifEqualsLexer | ifMatchesLexer | ifContainsLexer)

    val elseIfEqualsLexer: Parser[ElseIfEqualsToken] = ElseIfEqualsLexer().asInstanceOf[Parser[ElseIfEqualsToken]]
    val elseIfMatchesLexer: Parser[ElseIfMatchesToken] = ElseIfMatchesLexer().asInstanceOf[Parser[ElseIfMatchesToken]]
    val elseIfContainsLexer: Parser[ElseIfContainsToken] = ElseIfContainsLexer().asInstanceOf[Parser[ElseIfContainsToken]]

    val elseLexer: Parser[ELSE.type] = ElseLexer().asInstanceOf[Parser[ELSE.type]]
    val endifLexer: Parser[ENDIF.type] = EndifLexer().asInstanceOf[Parser[ENDIF.type]]
    val else_parsers = (elseIfEqualsLexer | elseIfMatchesLexer | elseIfContainsLexer | elseLexer | endifLexer)
    (if_parsers | else_parsers)
  }

  private def statementParsers(): Parser[StatementToken] = {
    val addLexer: Parser[AddStatementToken] = AddStatementLexer().asInstanceOf[Parser[AddStatementToken]]
    val removeLexer: Parser[RemoveStatementToken] = RemoveStatementLexer().asInstanceOf[Parser[RemoveStatementToken]]
    val enableLexer: Parser[EnableStatementToken] = EnableStatementLexer().asInstanceOf[Parser[EnableStatementToken]]
    val disableLexer: Parser[DisableStatementToken] = DisableStatementLexer().asInstanceOf[Parser[DisableStatementToken]]
    (addLexer | removeLexer | enableLexer | disableLexer)
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
    val exit: Parser[EndSection.type] = EndSectionLexer().asInstanceOf[Parser[EndSection.type]]
    val parser = phrase(rep1(exit | sectionParsers() | logicParsers() | statementParsers()))

    parse(parser, line) match {
      case NoSuccess(msg, next) =>
        println(s"Could not parse line = [$line]. error=${msg}")
        None

      case Success(resultList, next) =>
        println(s"Successfully parsed line = [$line].")
        resultList.headOption
    }
  }
}
