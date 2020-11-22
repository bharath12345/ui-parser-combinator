package tutorial.webapp

import tutorial.webapp.Lexer._

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

trait TokenCommon {
  val id_name_r = """\s+id\s*=\s*(.*?)\s+name\s*=\s*"(.*?)"\s*"""
  val req_enb_disp_r = """\s*(required)?\s*(disabled)?\s*(nodisplay)?"""
}

object Lexer {

  sealed trait Token

  case object EXIT extends Token {
    override def toString: String = "EXIT()"
  }

  sealed trait SectionToken extends Token
  case class HeadingToken(name: String, title: Option[String]) extends SectionToken
  case class TextInputToken(name: String, id: String, value: Option[String],
                            required: Boolean, enabled: Boolean, display: Boolean) extends SectionToken
  case class SelectToken(name: String, id: String, values: List[String],
                         required: Boolean, enabled: Boolean, display: Boolean) extends SectionToken with TokenCommon


  sealed trait LogicTokens extends Token

  case class LogicBlock(logicTokens: List[LogicTokens], blocks: List[LogicBlock]) extends Token

  abstract sealed class EqualsToken(st: SectionToken, str: String, statements: List[Statements]) extends LogicTokens
  abstract sealed class MatchesToken(st: SectionToken, regex: Regex, statements: List[Statements]) extends LogicTokens
  abstract sealed class ContainsToken(st: SectionToken, str: String, statements: List[Statements]) extends LogicTokens

  case class IfEqualsToken(st: SectionToken, str: String, statements: List[Statements]) extends EqualsToken(st, str, statements)
  case class IfMatchesToken(st: SectionToken, regex: Regex, statements: List[Statements]) extends MatchesToken(st, regex, statements)
  case class IfContainsToken(st: SectionToken, str: String, statements: List[Statements]) extends ContainsToken(st, str, statements)

  case class ElseIfEqualsToken(st: SectionToken, str: String, statements: List[Statements]) extends EqualsToken(st, str, statements)
  case class ElseIfMatchesToken(st: SectionToken, regex: Regex, statements: List[Statements]) extends MatchesToken(st, regex, statements)
  case class ElseIfContainsToken(st: SectionToken, str: String, statements: List[Statements]) extends ContainsToken(st, str, statements)

  sealed trait Statements extends Token
  case class AddStatement(st: SectionToken) extends Statements
  case class RemoveStatement(st: SectionToken) extends Statements
  case class EnableStatement(st: SectionToken) extends Statements
  case class DisableStatement(st: SectionToken) extends Statements

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

  override protected val regex: Regex = s"""^DEFINE\\s+SECTION\\s+(\\w+)\\s*(title\\s*=\\s*"(.*?)")?$$""".r

  def apply(): Parser[SectionToken] =
    regexMatch(regex) ^^ (m => get(m.subgroups.map(x => if (x == null) x else x.trim): _*))

  protected def get(values: String*): SectionToken = {
    val list = values.toArray
    var headingToken = HeadingToken(list(0), None)
    if(list(1) != null) headingToken = headingToken.copy(title = Option(list(2)))
    headingToken
  }
}

object TextInputTokenLexer extends RegexLexer with TokenCommon {
  private val optional_value_r = """(value\s*=\s*"(.*?)")?"""
  override protected val regex: Regex = s"""^TEXTINPUT${id_name_r}${optional_value_r}${req_enb_disp_r}$$""".r

  def apply(): Parser[TextInputToken] =
    regexMatch(regex) ^^ (m => get(m.subgroups.map(x => if (x == null) x else x.trim): _*))

  protected def get(values: String*): TextInputToken = {
    val list = values.toArray
    val size = 7
    if(list.length != size) {
      println(s"error: more than $size elements in parse result ${list.toList}")
      null
    } else {
      println(s"text input input values = ${list.toList}")
      var ti = TextInputToken(list(0), list(1), None, false, false, false)
      if(list(2) != null) ti = ti.copy(value = Option(list(3)))
      if(list(4) != null) ti = ti.copy(required = true)
      if(list(5) == null) ti = ti.copy(enabled = true)
      if(list(6) == null) ti = ti.copy(display = true)
      ti
    }
  }
}

object SelectLexer extends RegexLexer with TokenCommon {
  private val options_r = """(options\s*=\s*\[(.*?)\])?"""
  override protected val regex: Regex = s"""SELECT${id_name_r}${options_r}${req_enb_disp_r}$$""".r

  def apply(): Parser[SelectToken] =
    regexMatch(regex) ^^ (m => get(m.subgroups.map(x => if (x == null) x else x.trim): _*))

  protected def get(values: String*): SelectToken = {
    val list = values.toArray
    val size = 7
    if(list.length != size) {
      println(s"error: more than $size elements in parse result ${list.toList}")
      null
    } else {
      println(s"text input input values = ${list.toList}")
      var st = SelectToken(list(0), list(1), List(), false, false, false)
      if(list(2) != null) {
        val x: List[String] = list(3).split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)", -1).toList
        val y: List[String] = x.map(_.replace("\"", ""))
        st = st.copy(values = y)
      }
      if(list(4) != null) st = st.copy(required = true)
      if(list(5) == null) st = st.copy(enabled = true)
      if(list(6) == null) st = st.copy(display = true)
      st
    }
  }
}