package tutorial.webapp

import tutorial.webapp.Lexer.{HeadingToken, SectionToken, TextInputToken, Token}

object ParserApp {

  val grammar: Array[String] =
    """# this is a comment
      |DEFINE SECTION ContentBranding
      |  HEADING title="Content branding details"
      |  TEXTINPUT name="One" id="textInput1"
      |  TEXTINPUT name="Two" id="textInput2" value="Type here"
      |  TEXTINPUT name="Three" id="textInput3" value="Type here" required=true
      |;
      |""".stripMargin.split("\\n")

  def main(args: Array[String]): Unit = {
    println("Hello test!")
    val parser = new SchemaParser()
    val tokens: List[Token] = parser.getTokens(grammar)

    val render = new Render(tokens.head.asInstanceOf[SectionToken])
    tokens.tail.foreach (render.apply)
  }
}
