package tutorial.webapp

import tutorial.webapp.Lexer.SectionToken

import scala.util.matching.Regex

object AST {

  sealed trait Logic
  abstract sealed class EqualsLogic(st: SectionToken, str: String, statements: List[Statements]) extends Logic
  abstract sealed class MatchesLogic(st: SectionToken, regex: Regex, statements: List[Statements]) extends Logic
  abstract sealed class ContainsLogic(st: SectionToken, str: String, statements: List[Statements]) extends Logic

  case class IfEqualsLogic(st: SectionToken, str: String, statements: List[Statements]) extends EqualsLogic(st, str, statements)
  case class IfMatchesLogic(st: SectionToken, regex: Regex, statements: List[Statements]) extends MatchesLogic(st, regex, statements)
  case class IfContainsLogic(st: SectionToken, str: String, statements: List[Statements]) extends ContainsLogic(st, str, statements)

  case class ElseIfEqualsLogic(st: SectionToken, str: String, statements: List[Statements]) extends EqualsLogic(st, str, statements)
  case class ElseIfMatchesLogic(st: SectionToken, regex: Regex, statements: List[Statements]) extends MatchesLogic(st, regex, statements)
  case class ElseIfContainsLogic(st: SectionToken, str: String, statements: List[Statements]) extends ContainsLogic(st, str, statements)

  sealed trait Statements
  case class AddStatement(st: SectionToken) extends Statements
  case class RemoveStatement(st: SectionToken) extends Statements
  case class EnableStatement(st: SectionToken) extends Statements
  case class DisableStatement(st: SectionToken) extends Statements

  case class LogicBlock(logic: List[Logic], blocks: List[LogicBlock])

  def buildAST(tokens: List[Logic]): AST = {

  }
}

class AST {

}
