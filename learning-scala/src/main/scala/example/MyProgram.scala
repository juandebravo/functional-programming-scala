package example

import scala.annotation.tailrec

object MyProgram {
  def abs(x: Int): Int = {
    if (x < 0) -x
    else x
  }

  def factorial(x: Int): Int = {
    def loop(x: Int, acc: Int): Int = {
      if (x <= 0) acc
      else loop(x-1, x*acc)
    }
    loop(x, 1)
  }

  def fibonacci(n: Int): Int = {
    def loop(n: Int, prev: Int, acc: Int): Int = {
      if (n <= 0) prev
      else loop(n - 1, acc, prev + acc)
    }
    loop(n, 0, 1)
  }

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n >= as.length) true
      else if (gt(as(n), as(n-1))) loop(n+1)
      else false
    }

    if (as.length <= 1) true
    else loop(1)
  }

  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    a => b => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a) => f(g(a))
  }

  // 3.13
  def foldRight[A, B](as: MyList[A], acc: B, f: (A, B) => B): B =
    foldLeft(reverse(as), acc, (b:B, a:A) => f(a,b))

  // 3.10
  @tailrec
  private def foldLeft[A, B](as: MyList[A], acc: B, f: (B, A) => B): B = as match {
    case Nil => acc
    case Cons(x, xs) => foldLeft(xs, f(acc, x), f )
  }

  def sum(ints: MyList[Int]): Int = foldLeft(ints, 0, (a: Int, b: Int) => a + b)

  def product(ds: MyList[Double]): Double = foldLeft(ds, 1.0, (a: Double, b: Double) => a * b)

  // 3.9 - 3.11
  def length[A](as: MyList[A]): Int = foldLeft(as, 0, (acc:Int, _: A) => acc + 1 )

  // 3.12
  def reverse[A](as: MyList[A]): MyList[A] = foldLeft(as, Nil, (acc: MyList[A], a: A) => Cons(a, acc))

  def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] =
    foldRight(a1, a2, (xs: A, x: MyList[A]) => Cons(xs, x))

  // 3.15
  def flatten[A](l: MyList[MyList[A]]): MyList[A] =
    foldRight(l, Nil, (a1: MyList[A], a2: MyList[A]) => append(a1, a2))

  // 3.16
  def increaseBy1: MyList[Int] => MyList[Int] = curry(increaseByN)(1)

  private def increaseByN(n: Int, l: MyList[Int]): MyList[Int] =
    foldRight(l, Nil, (xs: Int, acc: MyList[Int]) => Cons(xs + n, acc))

  // 3.17
  def dToString(as: MyList[Double]): MyList[String] =
    foldRight(as, Nil, (xs: Double, acc: MyList[String]) => Cons(xs.toString, acc))

  // 3.18
  def map[A, B](as: MyList[A], f: A => B): MyList[B] =
    foldRight(as, Nil, (xs: A, acc: MyList[B]) => Cons(f(xs), acc))

  // 3.19
  def filter[A](as: MyList[A], f: A => Boolean): MyList[A] =
    foldRight(as, Nil, (xs: A, acc: MyList[A]) => if (f(xs)) Cons(xs, acc) else acc)

  // 3.20
  def flatMap[A, B](as: MyList[A], f: A => MyList[B]): MyList[B] =
    foldRight(as, Nil, (xs: A, acc: MyList[B]) => append(f(xs), acc))

  // 3.21
  def filterWithFlatMap[A](as: MyList[A], f: A => Boolean): MyList[A] =
    flatMap(as, (a: A) => if (f(a)) MyList(a) else Nil)

  // 3.22
  def sumElements(a: MyList[Int], b: MyList[Int]): MyList[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1+h2, sumElements(t1, t2))
  }

  // 3.23
  def zipWith[A, B, C](a: MyList[A], b: MyList[B], f: (A, B) => C): MyList[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2, f))
  }

  def main(args: Array[String]): Unit = {
  }
}