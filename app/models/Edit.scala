package models

import models.Utils.AugmentedString
import models.Utils.AugmentedSeq

object Edits {
  type Text = Seq[String]

  implicit class AugmentedText(value: Text) {
    def asString: String = value.mkString("\n")
  }


  case class TextPosition(line: Int, row: Int)

  trait TextEdit {
    def textValue: Text
    def rebase(on: TextEdit): TextEdit
  }

  case class Insert(pos: TextPosition, newText: String, basedOn: TextEdit) extends TextEdit {
    val textValue = {
      if (newText.contains('\n')) {
        println("blabla")
        val (sameLine, newLine) = newText.span(_ != '\n')
        val newLineWithoutTrailingNewLine = newLine.dropRight(1)
        val oldLine = basedOn.textValue(pos.line)
        val step1: Seq[String] = basedOn.textValue.updated(pos.line, oldLine.insert(pos.row, sameLine))
        step1.insert(pos.line + 1, newLineWithoutTrailingNewLine)
      }
      else {
        val oldLine = basedOn.textValue(pos.line)
        basedOn.textValue.updated(pos.line, oldLine.insert(pos.row, newText))
      }
    }
    def rebase(on: TextEdit): Insert = ???
      /*if (basedOn == on) this
      else {
        on match {
          case Insert(pos, newChar, basedOn) => {
            val partialRebase = rebase(basedOn)
            if (pos == this.pos) on
            else if (pos > this.pos) this.copy(basedOn = on)
            else if (pos < this.pos) this.copy(basedOn = on, pos = this.pos + 1)
          }
          case Delete(pos, newChar, basedOn) => {
            val partialRebase = rebase(basedOn)
            if (pos == this.pos) on
            else if (pos > this.pos) this.copy(basedOn = on)
            else if (pos < this.pos) this.copy(basedOn = on, pos = this.pos - 1)
          }
        }
      }*/
  }
  case class Delete(pos: TextPosition, nb: Int, basedOn: TextEdit) extends TextEdit {
    def textValue = {
      val oldLine = basedOn.textValue(pos.line)
      basedOn.textValue.updated(pos.line, oldLine.remove(pos.row - nb, nb))
    }
    def rebase(on: TextEdit): Delete = ???
  }
  case class Merge(from: Set[TextEdit]) {
    def textValue = from.reduce(_.rebase(_)).textValue
  }
  object NilEdit extends TextEdit {
    def textValue = List("")
    def rebase(on: TextEdit) = on
  }
}
