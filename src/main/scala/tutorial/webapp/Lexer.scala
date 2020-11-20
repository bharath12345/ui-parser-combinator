package tutorial.webapp

import tutorial.webapp.Lexer._

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object Lexer {

  sealed trait Token

  case object EXIT extends Token {
    override def toString: String = "EXIT()"
  }

  sealed trait SectionTokens extends Token

  case class SectionToken(name: String) extends SectionTokens

  case class HeadingToken(name: String) extends SectionTokens

  case class TextInputToken(name: String, id: String, value: Option[String], required: Boolean) extends SectionTokens

}

trait RegexLexer extends RegexParsers {

  protected def regexMatch(regex: Regex): Parser[Regex.Match] = new Parser[Regex.Match] {
    def apply(in: Input): ParseResult[Regex.Match] = {
      if (in.atEnd) {
        Failure(s"string matching regex '$regex' expected but end hit", in.rest)
      } else {
        regex findPrefixMatchOf in.source match {
          case Some(matched) =>
            Success(matched, in.drop(in.source.length()))
          case None =>
            Failure(s"string matching regex '$regex' expected but '${in.source}' found", in.rest)
        }
      }
    }
  }

  protected val regex: Regex

}

object ExitLexer extends RegexLexer {

  def apply(): Parser[EXIT.type] = {
    regexMatch(regex) ^^ { case m => EXIT }
  }

  override protected val regex: Regex = """\s*;\s*""".r
}

object SectionLexer extends RegexLexer {

  override protected val regex: Regex = s"""^DEFINE\\s+SECTION\\s+(\\w+)$$""".r

  def apply(): Parser[SectionToken] =
    regexMatch(regex) ^^ (m => get(m.subgroups.map(x => if (x == null) x else x.trim): _*))

  protected def get(values: String*): SectionToken =
    SectionToken(values.toList.head)
}

object HeadingLexer extends RegexLexer {

  override protected val regex: Regex = s"""^HEADING\\s+title\\s*=\\s*"(.*?)"$$""".r

  def apply(): Parser[HeadingToken] =
    regexMatch(regex) ^^ (m => get(m.subgroups.map(x => if (x == null) x else x.trim): _*))

  protected def get(values: String*): HeadingToken =
    HeadingToken(values.toList.head)
}

object TextInputTokenLexer extends RegexLexer {
  override protected val regex: Regex = s"""^TEXTINPUT\\s+name\\s*=\\s*"(.*?)"\\s+id\\s*=\\s*"(.*?)"\\s+(.*?)(value\\s*=\\s*"(.*?)")?\\s*(required\\s*=\\s*(.*?))?""".r

  def apply(): Parser[TextInputToken] =
    regexMatch(regex) ^^ (m => get(m.subgroups.map(x => if (x == null) x else x.trim): _*))

  protected def get(values: String*): TextInputToken = {
    val list = values.toArray
    list.length match {
      case 2 => TextInputToken(list(0), list(1), None, false)
      case 3 => TextInputToken(list(0), list(1), Option(list(2)), false)
      case 4 => TextInputToken(list(0), list(1), Option(list(2)), list(3).toBoolean)
    }
  }
}