package example

import org.scalatest.{FlatSpec, Matchers}

// 4.3
def map2[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] = (a, b) match {
  case (Some(a), Some(b)) => Some(f(a, b))
  case _ => None
}

class MyOptionSpec extends FlatSpec with Matchers {
  "MyOption" should "be able to initialize to Some" in {
    val value = Some(34)
    value.get shouldEqual 34
  }

  "MyOption" should "be able to initialize to None" in {
    val value: MyOption[Nothing] = None
    value shouldEqual None
  }

  "map" should "work with Some values" in {
    val value = Some(30)
    value.map(_ * 4) shouldEqual Some(120)
  }

  "getOrElse" should "work" in {
    val value: MyOption[Nothing] = None
    value.getOrElse(10) shouldEqual 10
    Some(15).getOrElse(20) shouldEqual 15
  }

  "flatMap" should "work" in {
    val value = Some(30)
    def f(x: Int): MyOption[Int] = Some(x * 4)

    value.flatMap(f) shouldEqual Some(120)
  }

  "map2" should "work" in {
    map2(Some(1), Some(2)) {_+_} shouldEqual Some(3)

    map2(Some(1), Some(2))(_ + _) shouldEqual Some(3)
  }

//  "sequence" should "work" in {
//    def sequence[A](as: List[MyOption[A]]): MyOption[List[A]] =
//      as.foldRight[MyOption[List[A]]](Some(Nil))(a, acc) => map2(a, acc(_::_)))
//  }

//  "orElse" should "work" in {
//    val value = Some(30)
//    value.orElse(10) shouldEqual 30
//  }

}
