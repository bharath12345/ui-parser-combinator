package tutorial.webapp

import tutorial.webapp.AST.LogicBlock
import tutorial.webapp.Lexer._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import scala.annotation.tailrec

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

    println(s"section list = $sectionList")

    def findSection(name: String): Option[SectionToken] = sectionList.find(x => x.id == name)

    def isLogical(token: Token): Boolean =
      if (token.isInstanceOf[IfEqualsToken] || token.isInstanceOf[IfMatchesToken] || token.isInstanceOf[IfContainsToken] ||
        token.isInstanceOf[ElseIfEqualsToken] || token.isInstanceOf[ElseIfMatchesToken] || token.isInstanceOf[ElseIfContainsToken] ||
        token.isInstanceOf[ELSE.type]) true else false

    def isStatement(token: Token): Boolean = if(token.isInstanceOf[AddStatementToken] || token.isInstanceOf[RemoveStatementToken] ||
      token.isInstanceOf[EnableStatementToken] || token.isInstanceOf[DisableStatementToken]) true else false

    def recursive(logicTokens: List[Token], wellFormed: List[(LogicToken, List[StatementToken])]): List[(LogicToken, List[StatementToken])] = {
      logicTokens match {
        case Nil =>
          wellFormed

        case head +: tail =>
          println(s"head = $head, tail = $tail")
          val statements: ListBuffer[StatementToken] = ListBuffer()
          if(isLogical(head)) {
            val logicToken = head.asInstanceOf[LogicToken]
            if(tail != Nil) {
              @tailrec def loop(stmtTokens: List[Token]): List[(LogicToken, List[StatementToken])] = {
                println(s"stmtTokens = $stmtTokens")
                stmtTokens match {
                  case Nil =>
                    wellFormed

                  case (h +: t) =>
                    val newWellFormed = wellFormed :+ (logicToken -> statements.toList)
                    println(s"new well formed = $newWellFormed")
                    if (isLogical(h)) {
                      recursive(h +: t, newWellFormed)
                    } else if (isStatement(h)) {
                      statements.addOne(h.asInstanceOf[StatementToken])
                      loop(t)
                    } else {
                      wellFormed
                    }
                }
              }
              loop(tail)
            } else {
              wellFormed
            }
          } else {
            println(s"head token not logical: $head")
            wellFormed
          }
      }
    }

    val wellFormed = recursive(tokens.dropWhile(t => t.isInstanceOf[SectionToken]), List())
    println(s"well formed = $wellFormed")

    val xxx: List[(Logic, List[Statement])] = wellFormed.flatMap { lb =>
      val logic: Option[Logic] = lb._1 match {
        case iet: IfEqualsToken => findSection(iet.id).map {IfEqualsLogic(_, iet.str, List())}
        case imt: IfMatchesToken => findSection(imt.id).map {IfMatchesLogic(_, imt.regex, List())}
        case ict: IfContainsToken => findSection(ict.id).map {IfContainsLogic(_, ict.str, List())}
        case eet: ElseIfEqualsToken => findSection(eet.id).map {ElseIfEqualsLogic(_, eet.str, List())}
        case emt: ElseIfMatchesToken => findSection(emt.id).map {ElseIfMatchesLogic(_, emt.regex, List())}
        case ect: ElseIfContainsToken => findSection(ect.id).map {ElseIfContainsLogic(_, ect.id, List())}
      }

      val yyy: Option[(Logic, List[Statement])] = logic.map { l =>
        l -> lb._2.flatMap {
          case add: AddStatementToken => findSection(add.id).map {AddStatement}
          case remove: RemoveStatementToken => findSection(remove.id).map {RemoveStatement}
          case enable: EnableStatementToken => findSection(enable.id).map {EnableStatement}
          case disable: DisableStatementToken => findSection(disable.id).map {DisableStatement}
        }
      }
      yyy
    }

    val zzz: List[Logic] = xxx.map { case (logic, stmts) =>
      logic match {
        case i: IfEqualsLogic => i.copy(statements = stmts)
        case i: IfMatchesLogic => i.copy(statements = stmts)
        case i: IfContainsLogic => i.copy(statements = stmts)
        case i: ElseIfEqualsLogic => i.copy(statements = stmts)
        case i: ElseIfMatchesLogic => i.copy(statements = stmts)
        case i: ElseIfContainsLogic => i.copy(statements = stmts)
      }
    }

    println(s"zzz = $zzz")

    AST(sectionList, LogicBlock(zzz))
  }
}

case class AST(sections: List[SectionToken], lb: LogicBlock)
