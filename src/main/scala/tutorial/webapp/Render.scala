package tutorial.webapp

import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.raw.Element
import tutorial.webapp.AST._
import tutorial.webapp.Lexer._
import org.scalajs.dom.{Event, _}
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.raw.HTMLInputElement
import scala.scalajs.js.annotation.JSExportTopLevel

object Render {

  private val form = {
    val div = document.createElement("div")
    div.classList.add("container")
    document.body.appendChild(div)

    val form = document.createElement("form")
    div.appendChild(form)
    form
  }

  def render(ast: AST): Any = {
    ast.sections.foreach { token =>
      println(s"incoming token = $token")
      token match {
        case ht: HeadingToken => renderHeading(ht)
        case txt: TextInputToken => renderTextInput(txt)
        case sel: SelectToken => renderSelect(sel)
      }
    }

    ast.lb.logics.foreach(renderLogic)
  }

  private def renderHeading(ht: HeadingToken): Unit = {
    val heading = document.createElement("h3")
    heading.id = ht.id
    ht.title.foreach(x => heading.innerText = x)
    form.appendChild(heading)
  }

  private def addRowCol(id: String, name: String) = {
    val section = document.createElement("div")
    section.classList.add("row")
    section.classList.add("mb-3")
    form.appendChild(section)

    val label = document.createElement("label")
    label.classList.add("col-sm-2")
    label.classList.add("col-form-label")
    label.setAttribute("for", id)
    label.innerText = name
    section.appendChild(label)

    val column = document.createElement("div")
    column.classList.add("col-sm-10")
    section.appendChild(column)
    column
  }

  private def renderTextInput(txt: TextInputToken): Unit = {
    val column = addRowCol(txt.id, txt.name)

    val input = document.createElement("input")
    input.classList.add("form-control")
    input.id = txt.id
    txt.value.foreach(v => input.setAttribute("value", v))
    column.appendChild(input)
  }

  private def renderSelect(sel: SelectToken): Unit = {
    val column = addRowCol(sel.id, sel.name)

    val select = document.createElement("select")
    select.classList.add("form-select")
    select.classList.add("form-select-sm")
    select.setAttribute("aria-label", ".form-select-lg example")
    column.appendChild(select)

    sel.values.zipWithIndex.foreach { case(value, count) =>
      println(s"adding option for value = $value")
      val option = document.createElement("option")
      if(count == 0) option.setAttribute("selected", "")
      else option.setAttribute("value", count.toString)
      option.innerText = value
      select.appendChild(option)
    }
  }

  private def renderLogic(logic: Logic): Unit = {
    logic match {
      case iet: IfEqualsLogic =>
        println(s"adding listener on ${iet.st.id}")
        val elem = document.getElementById(iet.st.id)
        elem.addEventListener("input", { (e: dom.KeyboardEvent) =>
          e.currentTarget match {
            case input: HTMLInputElement =>
              println(s"on click firing. event = ${input.value}")
              if(input.value == iet.str ) {
                println(s"you entered test")
                val disableStmt = iet.statements.head.asInstanceOf[DisableStatement]
                val target = document.getElementById(disableStmt.st.id)
                target.setAttribute("disabled", "true")
              }
          }
        })

      case imt: IfMatchesLogic =>
      case ict: IfContainsLogic =>
      case eet: ElseIfEqualsLogic =>
      case emt: ElseIfMatchesLogic =>
      case ect: ElseIfContainsLogic =>
    }
  }
}
