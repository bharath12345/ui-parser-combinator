package tutorial.webapp

import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.raw.Element
import tutorial.webapp.Lexer.{EXIT, HeadingToken, SectionToken, SelectToken, TextInputToken, Token}

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

  def render(token: Token): Any = {
    println(s"incoming token = $token")
    token match {
      case ht: HeadingToken => renderHeading(ht)
      case txt: TextInputToken => renderTextInput(txt)
      case sel: SelectToken => renderSelect(sel)
      case EXIT =>
    }
  }

  private def renderHeading(ht: HeadingToken): Unit = {
    val heading = document.createElement("h3")
    heading.id = ht.name
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
}
