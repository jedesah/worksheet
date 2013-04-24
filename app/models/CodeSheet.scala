package models

import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import scala.tools.reflect.ToolBoxError

trait CodeSheet {
    def computeResults(code: String): List[String]
}

object ScalaCodeSheet extends CodeSheet {

    val cm = ru.runtimeMirror(getClass.getClassLoader)
    val toolBox = cm.mkToolBox()

    def computeResults(code: String): List[String] = {
        var accu = ""
        code.lines.toList.map{ line =>

          try {

            val subTree = toolBox.parse(line)
            println(showRaw(subTree))
            val oldAccu = accu
            accu = accu + "\n" + line
            subTree match {
              case ValDef(_, newTermName, _, expr) =>
            newTermName + " = " + toolBox.eval(toolBox.parse(oldAccu + "\n" + expr.toString)).toString
              case _            => toolBox.eval(toolBox.parse(accu)).toString
            }
          } catch {
            case ToolBoxError(msg, cause) => {
              msg.dropWhile(_ != ':').drop(2)
            }
          }
        }
    }
}

object JrCodeSheet extends CodeSheet {
    import LanguageAST._
    def computeResults(code: String): List[String] = {
      Parser.parse(code).evaluateOutput(Map(), Any)
    }
}