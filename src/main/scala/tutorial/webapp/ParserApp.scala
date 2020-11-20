package tutorial.webapp

import tutorial.webapp.Lexer.{HeadingToken, SectionToken, TextInputToken, Token}

object ParserApp {
  def main(args: Array[String]): Unit = {
    println("Hello test!")
    val parser = new SchemaParser()
    val tokens: List[Token] = parser.getTokens()
    tokens.foreach (Render.apply)
  }
}
