import MyList._

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

    def main(args: Array[String]): Unit = {
      /*
      println(isSorted(Array(1, 2, 3), (a: Int, b: Int) => a > b))
      println(isSorted(Array(1, 2, 1), (a: Int, b: Int) => a > b))
      println(isSorted(Array(3, 2, 1), (a: Int, b: Int) => a < b))
      println(isSorted(Array(1, 2, 3), (a: Int, b: Int) => a < b))
      println(isSorted(Array(1), (a: Int, b: Int) => a < b))
      */
      val sum = curry((a: Int, b: Int) => a + b)
      val incrBy1 = sum(1)
      println(incrBy1(10))
      println(MyList(1, 2, 3).size)
      println(MyList.size(1, 2, 3))
      println(MyList.size(MyList(1, 2, 3)))
      println(MyList().size)
    }
}