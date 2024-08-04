sealed trait MyList[+A] {
  def size: Int = {
    def loop(as: MyList[A], acc: Int): Int = {
      as match {
        case Nil => acc
        case Cons(_, t) => loop(t, acc + 1)
      }
    }
    loop(this, 0)
  }
}

case object Nil extends MyList[Nothing]
case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {
  def apply[A](as: A*): MyList[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def size[A](as: A*): Int = {
    def loop(acc: Int, as: A*): Int = {
      if (as.isEmpty) acc
      else loop(acc + 1, as.tail: _*)
    }
    loop(0, as: _*)
  }

  def size[A](as: MyList[A]): Int = as.size

  def tail[A](as: MyList[A]): MyList[A] = as match {
    case Nil => sus.error("message")
    case Cons(x, xs) => MyList(xs)
  }
}
