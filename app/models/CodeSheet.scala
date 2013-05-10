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
        val wholeParses =
          try{
            val wholeAST = toolBox.parse(code)
            true
        } catch {
          case _: Throwable => false
        }

        var accu = ""
        code.lines.toList.map{ line =>

            if (line == "" || line.startsWith("//")) ""
            else try {
              val oldAccu = accu
              accu = accu + "\n" + line
              val lineAST = toolBox.parse(line)
              try {
                lineAST match {
                  case block: Block => block.children.map(tree => toolBox.eval(Block(toolBox.parse(oldAccu),tree)).toString).mkString(" ; ")
                  case _ : ClassDef => ""
                  case _ : DefDef => ""
                  case ValDef(_, newTermName, _, expr) =>
                    newTermName + " = " + toolBox.eval(toolBox.parse(oldAccu + "\n" + expr.toString)).toString
                  case _            => toolBox.eval(toolBox.parse(accu)).toString
                }
              } catch {
                case ToolBoxError(msg, cause) => msg.dropWhile(_ != ':').drop(2)
                case _: Throwable => /* The reflection compiler might hit some assertions because we are stressing it a bit much */ ""
              }
            } catch {
                case ToolBoxError(msg, cause) => if (wholeParses) "" else msg.dropWhile(_ != ':').drop(2)
                case _: Throwable => /* The reflection compiler might hit some assertions because we are stressing it a bit much */ ""
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