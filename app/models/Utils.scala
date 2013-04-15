package models

object Utils {
  implicit class AugmentedSeq[A](value: Seq[A]) {
    def insert(pos: Int, newElem: A) = {
      val (begin, after) = value.splitAt(pos)
      (begin :+ newElem) ++ after
    }
    def insert(pos: Int, toAdd: Seq[A]) = {
      value.patch(pos, toAdd, 0)
    }
    def remove(pos: Int, nb: Int) = {
      value.patch(pos, "", nb)
    }
  }
  implicit class AugmentedString(value: String) {
    def insert(pos: Int, newChar: Char) = {
      val (begin, after) = value.splitAt(pos)
      (begin :+ newChar) ++ after
    }
    def insert(pos: Int, toAdd: String) = {
      value.patch(pos, toAdd, 0)
    }
    def remove(pos: Int, nb: Int) = {
      value.patch(pos, "", nb)
    }
  }
}
