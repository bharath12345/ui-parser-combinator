package tutorial.webapp

import org.scalajs.dom
import org.scalajs.dom.document
import tutorial.webapp.Lexer.{EXIT, HeadingToken, SectionToken, TextInputToken, Token}

import scala.scalajs.js.annotation.JSExportTopLevel

object Render {

  def apply(token: Token): Any = {
    token match {
      case st: SectionToken => renderSection(st)
      case ht: HeadingToken => renderHeading(ht)
      case txt: TextInputToken => renderTextInput(txt)
      case EXIT =>
    }
  }

  private def renderSection(st: SectionToken): Unit = {
    println(s"going to add section with title = ${st.name}")
    val div = document.createElement("div")
    div.textContent = st.name
    div.classList.add("row")
    div.classList.add("mb-3")
    //val only_form = document.getElementById("only-form")
    document.body.appendChild(div)
  }

  private def renderHeading(ht: HeadingToken): Unit = {

  }

  private def renderTextInput(txt: TextInputToken): Unit = {

  }
}
