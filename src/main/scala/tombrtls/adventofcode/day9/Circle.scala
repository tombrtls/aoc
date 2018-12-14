package tombrtls.adventofcode.day9

case class Circle[A](init: List[A], value: A, tail: List[A]) {

  def next: Circle[A] = tail match {
    case x :: xs => Circle(value :: init, x, xs)
    case Nil => {
      init.reverse match {
        case hd :: it => Circle(List(value), hd, it)
        case Nil => this
      }
    }
  }

  def prev: Circle[A] = init match {
    case x :: xs => Circle(xs, x, value :: tail)
    case Nil => {
      tail.reverse match {
        case hd :: it => Circle(it, hd, List(value))
        case Nil => this
      }
    }
  }

  def rotate(n: Int): Circle[A] = {
    if (n == 0)
      this
    else if (n > 0)
      next.rotate(n - 1)
    else
      prev.rotate(n + 1)
  }

  def inserted(elem: A): Circle[A] = Circle(init, elem, value :: tail)

  def removed(): Circle[A] = tail match {
    case tx :: txs => Circle(init, tx, txs)
    case Nil => {
      val ix :: ixs = init
      Circle(ixs, ix, tail)
    }
  }
}

object Circle {
  def apply[A](value: A): Circle[A] = Circle(List(), value, List())
}