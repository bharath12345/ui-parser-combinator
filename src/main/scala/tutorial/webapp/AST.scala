package tutorial.webapp

import tutorial.webapp.AST.LogicBlock
import tutorial.webapp.Lexer._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

object AST {

  sealed trait Logic

  abstract sealed class EqualsLogic(st: SectionToken, str: String, statements: List[Statement]) extends Logic
  abstract sealed class MatchesLogic(st: SectionToken, regex: Regex, statements: List[Statement]) extends Logic
  abstract sealed class ContainsLogic(st: SectionToken, str: String, statements: List[Statement]) extends Logic

  case class IfEqualsLogic(st: SectionToken, str: String, statements: List[Statement]) extends EqualsLogic(st, str, statements)
  case class IfMatchesLogic(st: SectionToken, regex: Regex, statements: List[Statement]) extends MatchesLogic(st, regex, statements)
  case class IfContainsLogic(st: SectionToken, str: String, statements: List[Statement]) extends ContainsLogic(st, str, statements)

  case class ElseIfEqualsLogic(st: SectionToken, str: String, statements: List[Statement]) extends EqualsLogic(st, str, statements)
  case class ElseIfMatchesLogic(st: SectionToken, regex: Regex, statements: List[Statement]) extends MatchesLogic(st, regex, statements)
  case class ElseIfContainsLogic(st: SectionToken, str: String, statements: List[Statement]) extends ContainsLogic(st, str, statements)

  sealed trait Statement
  case class AddStatement(st: SectionToken) extends Statement
  case class RemoveStatement(st: SectionToken) extends Statement
  case class EnableStatement(st: SectionToken) extends Statement
  case class DisableStatement(st: SectionToken) extends Statement

  case class LogicBlock(logics: List[Logic])

  def buildAST(tokens: List[Token]): AST = {
    val sectionTokens: List[Token] = tokens.takeWhile(t => t.isInstanceOf[SectionToken])
    val sectionList: List[SectionToken] = sectionTokens.map {
      case ht: HeadingToken => ht
      case txt: TextInputToken => txt
      case sel: SelectToken => sel
    }

    /*def findSection(name: String): Option[SectionToken] = sectionList.find(x => x.id == name)

    val logicTokens = tokens.dropWhile(t => t.isInstanceOf[SectionToken])
    val logicList: List[(LogicToken, Int)] = for {
      (token, i) <- logicTokens.zipWithIndex
      if token.isInstanceOf[IfEqualsToken] || token.isInstanceOf[IfMatchesToken] || token.isInstanceOf[IfContainsToken] ||
        token.isInstanceOf[ElseIfEqualsToken] || token.isInstanceOf[ElseIfMatchesToken] || token.isInstanceOf[ElseIfContainsToken] ||
        token.isInstanceOf[ELSE.type]
    } yield {
      token.asInstanceOf[LogicToken] -> i
    }

    val statementList: List[(StatementToken, Int)] = for {
      (token, i) <- logicTokens.zipWithIndex
      if token.isInstanceOf[AddStatementToken] || token.isInstanceOf[RemoveStatementToken] ||
        token.isInstanceOf[EnableStatementToken] || token.isInstanceOf[DisableStatementToken]
    } yield {
      token.asInstanceOf[StatementToken] -> i
    }

    var prevLogic: (LogicToken, Int) = null
    val grouped: List[(LogicToken, List[StatementToken])] = for(logic <- logicList.tail) yield {
      val filteredStatementGroup = {
        if(prevLogic == null) statementList
        else statementList.dropWhile(x => x._2 < prevLogic._2)
      }
      val statementGroup: List[StatementToken] = for {
        statement <- filteredStatementGroup
        if(statement._2 < logic._2)
      } yield statement._1
      val result = (prevLogic._1, statementGroup)
      prevLogic = logic
      result
    }

    val wellFormed: List[(LogicToken, List[StatementToken])] = (logicList.head._1 -> grouped.head._2) +: grouped.tail

    val mess: List[(Logic, List[Statement])] = wellFormed.flatMap { lb =>
      val logic: Option[Logic] = lb._1 match {
        case iet: IfEqualsToken => findSection(iet.id).map {IfEqualsLogic(_, iet.str, List())}
        case imt: IfMatchesToken => findSection(imt.id).map {IfMatchesLogic(_, imt.regex, List())}
        case ict: IfContainsToken => findSection(ict.id).map {IfContainsLogic(_, ict.str, List())}
        case eet: ElseIfEqualsToken => findSection(eet.id).map {ElseIfEqualsLogic(_, eet.str, List())}
        case emt: ElseIfMatchesToken => findSection(emt.id).map {ElseIfMatchesLogic(_, emt.regex, List())}
        case ect: ElseIfContainsToken => findSection(ect.id).map {ElseIfContainsLogic(_, ect.id, List())}
      }

      val xx: Option[(Logic, List[Statement])] = logic.map { l =>
        l -> lb._2.flatMap {
          case add: AddStatementToken => findSection(add.id).map {AddStatement}
          case remove: RemoveStatementToken => findSection(remove.id).map {RemoveStatement}
          case enable: EnableStatementToken => findSection(enable.id).map {EnableStatement}
          case disable: DisableStatementToken => findSection(disable.id).map {DisableStatement}
        }
      }
      xx
    }

    val yy: List[Logic] = mess.map { case (logic, stmts) =>
      logic match {
        case i: IfEqualsLogic => i.copy(statements = stmts)
        case i: IfMatchesLogic => i.copy(statements = stmts)
        case i: IfContainsLogic => i.copy(statements = stmts)
        case i: ElseIfEqualsLogic => i.copy(statements = stmts)
        case i: ElseIfMatchesLogic => i.copy(statements = stmts)
        case i: ElseIfContainsLogic => i.copy(statements = stmts)
      }
    }*/

    AST(sectionList, LogicBlock(List()))
  }
}

case class AST(sections: List[SectionToken], lb: LogicBlock)
