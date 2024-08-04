package example

sealed trait MyLazyList[+A] {
  def headOption: MyOption[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }
}

case object Empty extends MyLazyList[Nothing]

case class Cons[A](h: () => A, t: () => MyLazyList[A]) extends MyLazyList[A]

object MyLazyList {
  def cons[A](hd: => A, tl: => MyLazyList[A]): MyLazyList[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: MyLazyList[A] = Empty

  def apply[A](as: A*): MyLazyList[A] = {
    if (as.isEmpty) Empty
    else cons(as.head, apply(as.tail: _*))
  }
}