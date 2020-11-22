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

  case object EXIT extends Token

  sealed trait SectionToken extends Token
  case class HeadingToken(name: String, title: Option[String]) extends SectionToken
  case class TextInputToken(name: String, id: String, value: Option[String],
                            required: Boolean, enabled: Boolean, display: Boolean) extends SectionToken
  case class SelectToken(name: String, id: String, values: List[String],
                         required: Boolean, enabled: Boolean, display: Boolean) extends SectionToken with TokenCommon


  sealed trait LogicToken extends Token

  abstract sealed class EqualsToken(name: String, str: String) extends LogicToken
  abstract sealed class MatchesToken(name: String, regex: Regex) extends LogicToken
  abstract sealed class ContainsToken(name: String, str: String) extends LogicToken

  case class IfEqualsToken(name: String, str: String) extends EqualsToken(name, str)
  case class IfMatchesToken(name: String, regex: Regex) extends MatchesToken(name, regex)
  case class IfContainsToken(name: String, str: String) extends ContainsToken(name, str)

  case class ElseIfEqualsToken(name: String, str: String) extends EqualsToken(name, str)
  case class ElseIfMatchesToken(name: String, regex: Regex) extends MatchesToken(name, regex)
  case class ElseIfContainsToken(name: String, str: String) extends ContainsToken(name, str)

  case object ENDIF extends LogicToken
  case object ELSE extends LogicToken

  sealed trait Statement extends Token
  case class AddStatementToken(name: String) extends Statement
  case class RemoveStatementToken(name: String) extends Statement
  case class EnableStatementToken(name: String) extends Statement
  case class DisableStatementToken(name: String) extends Statement

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
    regexMatch(regex) ^^ (m => EXIT)
  }

  override protected val regex: Regex = ";".r
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

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////

object IfEqualsLexer extends RegexLexer {
  override protected val regex: Regex = s"""^IF\\s+(\\w+)\\s+EQUALS\\s+"(.*?)"$$""".r

  def apply(): Parser[IfEqualsToken] =
    regexMatch(regex) ^^ (m => get(m.subgroups.map(x => if (x == null) x else x.trim): _*))

  protected def get(values: String*): IfEqualsToken = {
    val list = values.toArray
    IfEqualsToken(list(0), list(1))
  }
}

object IfMatchesLexer extends RegexLexer {
  override protected val regex: Regex = s"""^IF\\s+(\\w+)\\s+MATCHES\\s+REGEX\((.*?)\)$$""".r

  def apply(): Parser[IfMatchesToken] =
    regexMatch(regex) ^^ (m => get(m.subgroups.map(x => if (x == null) x else x.trim): _*))

  protected def get(values: String*): IfMatchesToken = {
    val list = values.toArray
    IfMatchesToken(list(0), list(1).r)
  }
}

object IfContainsLexer extends RegexLexer {
  override protected val regex: Regex = s"""^IF\\s+(\\w+)\\s+CONTAINS\\s+"(.*?)"$$""".r

  def apply(): Parser[IfContainsToken] =
    regexMatch(regex) ^^ (m => get(m.subgroups.map(x => if (x == null) x else x.trim): _*))

  protected def get(values: String*): IfContainsToken = {
    val list = values.toArray
    IfContainsToken(list(0), list(1))
  }
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////

object ElseIfEqualsLexer extends RegexLexer {
  override protected val regex: Regex = s"""^IF\\s+(\\w+)\\s+EQUALS\\s+"(.*?)"$$""".r

  def apply(): Parser[ElseIfEqualsToken] =
    regexMatch(regex) ^^ (m => get(m.subgroups.map(x => if (x == null) x else x.trim): _*))

  protected def get(values: String*): ElseIfEqualsToken = {
    val list = values.toArray
    ElseIfEqualsToken(list(0), list(1))
  }
}


object ElseIfMatchesLexer extends RegexLexer {
  override protected val regex: Regex = s"""^IF\\s+(\\w+)\\s+MATCHES\\s+REGEX\((.*?)\)$$""".r

  def apply(): Parser[ElseIfMatchesToken] =
    regexMatch(regex) ^^ (m => get(m.subgroups.map(x => if (x == null) x else x.trim): _*))

  protected def get(values: String*): ElseIfMatchesToken = {
    val list = values.toArray
    ElseIfMatchesToken(list(0), list(1).r)
  }
}

object ElseIfContainsLexer extends RegexLexer {
  override protected val regex: Regex = s"""^IF\\s+(\\w+)\\s+CONTAINS\\s+"(.*?)"$$""".r

  def apply(): Parser[ElseIfContainsToken] =
    regexMatch(regex) ^^ (m => get(m.subgroups.map(x => if (x == null) x else x.trim): _*))

  protected def get(values: String*): ElseIfContainsToken = {
    val list = values.toArray
    ElseIfContainsToken(list(0), list(1))
  }
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////

object EndifLexer extends RegexLexer {

  def apply(): Parser[ENDIF.type] = {
    regexMatch(regex) ^^ (m => ENDIF)
  }

  override protected val regex: Regex = "ENDIF".r
}

object ElseLexer extends RegexLexer {

  def apply(): Parser[ELSE.type] = {
    regexMatch(regex) ^^ (m => ELSE)
  }

  override protected val regex: Regex = "ELSE".r
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////

