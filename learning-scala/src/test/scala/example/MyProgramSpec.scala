package example

import org.scalatest.{FlatSpec, Matchers}
import MyProgram.{
  curry, isSorted, sum, product, reverse,
  foldRight, append, flatten, increaseBy1,
  dToString, map, filter, flatMap, filterWithFlatMap,
  sumElements, zipWith
}

class MyProgramSpec extends FlatSpec with Matchers {
  "curry" should "work with two arguments" in {
    val sum = curry((a: Int, b: Int) => a + b)
    val incrBy1 = sum(1)
    incrBy1(2) shouldEqual 3
  }

  "curry" should "work with different types" in {
    val sum = curry((a: Int, b: Double) => (a + b).toString)
    val incrBy1 = sum(1)
    incrBy1(2.0) shouldEqual "3.0"
  }

  "isSorted" should "return true if list is sorted" in {
    isSorted(Array(1, 2, 3), (a: Int, b: Int) => a > b) shouldEqual true
  }

  "isSorted" should "return false if list is not sorted" in {
    isSorted(Array(1, 2, 1), (a: Int, b: Int) => a > b) shouldEqual false
  }

  "isSorted" should "return true if list is sorted desc" in {
    isSorted(Array(3, 2, 1), (a: Int, b: Int) => a < b) shouldEqual true
  }

  "isSorted" should "return false if list is not sorted desc" in {
    isSorted(Array(1, 2, 3), (a: Int, b: Int) => a < b) shouldEqual false
  }

  "isSorted" should "return true if list has a single item" in {
    isSorted(Array(1), (a: Int, b: Int) => a < b) shouldEqual true
  }

  "isSorted" should "return true if list is empty" in {
    isSorted(Array(), (a: Int, b: Int) => a < b) shouldEqual true
  }

  "sum" should "sum two integers" in {
    sum(MyList(1, 2)) shouldEqual 3
  }

  "product" should "product two doubles" in {
    product(MyList(2, 3)) shouldEqual 6
  }

  "length" should "work for integers" in {
    MyProgram.length(MyList(2, 3)) shouldEqual 2
  }

  "reverse" should "work for integers" in {
    reverse(MyList(2, 3)) shouldEqual MyList(3,2)
  }

  "foldRight" should "work for integers" in {
    foldRight(MyList(2, 3), 2, (a: Int, b: Int)=>  a+b) shouldEqual 7
  }

  "append" should "work for two lists" in {
    append(MyList(1,2), MyList(3, 4)) shouldEqual MyList(1,2,3,4)
  }

  "flatten" should "work for lists of lists" in {
    flatten(MyList(MyList(1, 2), MyList(3, 4))) shouldEqual MyList(1, 2, 3, 4)
  }

  "increaseBy1" should "work with a list of 3 elements" in {
    increaseBy1(MyList(1,3,5,7)) shouldEqual(MyList(2,4,6,8))
  }

  "increaseBy1" should "work with a list of 4 elements" in {
    increaseBy1(MyList(4, 2, 8, 1)) shouldEqual (MyList(5, 3, 9, 2))
  }

  "dToString" should "work with a list of 2 elements" in {
    dToString(MyList(4, 2)) shouldEqual (MyList("4.0", "2.0"))
  }

  "map" should "work with a list of 2 elements" in {
    map(MyList(4, 2), (a: Int) => a * 2) shouldEqual (MyList(8, 4))
  }

  "filter" should "filter odd numbers" in {
    filter(MyList(1, 4, 3, 2, 7), (a: Int) => a % 2 == 0) shouldEqual (MyList(4, 2))
  }

  "flatMap" should "combine map results" in {
    flatMap(MyList(1, 4), (a: Int) => MyList(a, a)) shouldEqual (MyList(1, 1, 4, 4))
  }

  "filterWithFlatMap" should "filter odd numbers" in {
    filterWithFlatMap(MyList(1, 4, 3, 2, 7), (a: Int) => a % 2 == 0) shouldEqual (MyList(4, 2))
  }

  "sumElements" should "work with lists with same number of elements" in {
    sumElements(MyList(1, 4, 3), MyList(2, 0, 4)) shouldEqual MyList(3,4,7)
  }

  "sumElements" should "work if first list is shorter" in {
    sumElements(MyList(1, 4), MyList(2, 0, 4)) shouldEqual MyList(3, 4)
  }

  "sumElements" should "work if second list is shorter" in {
    sumElements(MyList(1, 4, 3), MyList(2, 0)) shouldEqual MyList(3, 4)
  }

  "zipWith" should "work" in {
    zipWith(MyList(1,2,3), MyList(2.0, 4.0, 6.0), (a: Int, b: Double) => a*b) shouldEqual MyList(2.0, 8.0, 18.0)
  }

}
