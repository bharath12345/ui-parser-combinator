package tutorial.webapp

import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.raw.Element
import tutorial.webapp.Lexer.{EXIT, HeadingToken, SectionToken, TextInputToken, Token}

import scala.scalajs.js.annotation.JSExportTopLevel

class Render(st: SectionToken) {

  private val form = {
    val div = document.createElement("div")
    div.classList.add("container")
    document.body.appendChild(div)

    val form = document.createElement("form")
    div.appendChild(form)
    form
  }

  def apply(token: Token): Any = {
    println(s"incoming token = $token")
    token match {
      case ht: HeadingToken => renderHeading(ht)
      case txt: TextInputToken => renderTextInput(txt)
      case EXIT =>
    }
  }

  private def renderHeading(ht: HeadingToken): Unit = {
    val heading = document.createElement("h3")
    heading.innerText = ht.name
    form.appendChild(heading)
  }

  private def renderTextInput(txt: TextInputToken): Unit = {
    val section = document.createElement("div")
    section.id = st.name
    section.classList.add("row")
    section.classList.add("mb-3")
    form.appendChild(section)

    val label = document.createElement("label")
    label.classList.add("col-sm-2")
    label.classList.add("col-form-label")
    label.setAttribute("for", txt.id)
    label.innerText = txt.name
    section.appendChild(label)

    val column = document.createElement("div")
    column.classList.add("col-sm-10")
    section.appendChild(column)

    val input = document.createElement("input")
    input.classList.add("form-control")
    input.id = txt.id
    txt.value.foreach(v => input.setAttribute("value", v))
    column.appendChild(input)
  }
}
