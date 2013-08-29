package models

trait CodeSheet {
    def computeResults(code: String): List[String]
}

object JrCodeSheet extends CodeSheet {
    import LanguageAST._
    def computeResults(code: String): List[String] = {
      Parser.parse(code).evaluateOutput(Map(), Any)
    }
}