package tutorial.webapp

import tutorial.webapp.Lexer.Token

object ParserApp {

  val grammar: Array[String] =
    """# this is a comment
      |DEFINE SECTION ContentBranding title="Content branding details"
      |  TEXTINPUT id=textInput1 name="Textbox One"
      |  TEXTINPUT id=textInput2 name="Textbox Two"   value="Type here" disabled
      |  TEXTINPUT id=textInput3 name="Textbox Three" value="Type here" required
      |  SELECT id=select1 name="Select City" options=[ "Ford", "BMW", "Fiat" ]
      |  TEXTINPUT id=textInput4 name="Textbox Four" value="Type here" nodisplay
      |
      |  IF textInput1 EQUALS "test"
      |    DISABLE textInput3
      |  ELSIF textInput1 MATCHES REGEX((mon|tues|wednes|thurs|fri|satur|sun)day)
      |    ENABLE textInput2
      |  ELSIF textInput3 CONTAINS "kumar"
      |    ADD textInput4
      |  ELSE
      |    # maybe something here
      |  ENDIF
      |;
      |""".stripMargin.split("\\n")

  def main(args: Array[String]): Unit = {
    println("Hello test!")
    val parser = new SchemaParser()
    val tokens: List[Token] = parser.getTokens(grammar)
    val ast: AST = AST.buildAST(tokens)
    Render.render(ast)
  }
}
