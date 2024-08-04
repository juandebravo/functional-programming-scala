package example

import scala.annotation.tailrec

sealed trait MyList[+A] {
  def size: Int = {
    @tailrec
    def loop(as: MyList[A], acc: Int): Int = as match {
        case Nil => acc
        case Cons(_, t) => loop(t, acc + 1)
      }
    loop(this, 0)
  }

  // 3.2
  def tail(): MyList[A] = this match {
    case Nil => sys.error("message")
    case Cons(_, xs) => xs
  }

  // 3.3
  def setHead[A](x: A) = this match {
    case Nil => sys.error("message")
    case Cons(_, xs) => Cons(x, xs)
  }

  // 3.4
  def drop(n: Int): MyList[A] = {
    @tailrec
    def loop(as: MyList[A], n: Int): MyList[A] = {
      if (n <= 0) as
      else as match {
        case Cons(_, xs) => loop(xs, n - 1)
        case Nil => Nil
      }
    }
    loop(this, n)
  }

  // 3.5
  def dropWhile(f: A => Boolean): MyList[A] = {
    @tailrec
    def loop(as: MyList[A]): MyList[A] = as match {
      case Nil => Nil
      case Cons(x, xs) if f(x) => loop(xs)
      case _ => as
    }
    loop(this)
  }

  // 3.6
  def init(): MyList[A] = {
    def loop(as: MyList[A]): MyList[A] = as match {
      case Nil => sys.error("init of empty Nil")
      // last message of a MyList instance is Cons(x, Nil)
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, loop(xs))
    }
    loop(this)
  }
}

case object Nil extends MyList[Nothing]
case class Cons[+A](head: A, xs: MyList[A]) extends MyList[A]

object MyList {
  def apply[A](as: A*): MyList[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def size[A](as: A*): Int = MyList(as).size

  def size[A](as: MyList[A]): Int = as.size
}