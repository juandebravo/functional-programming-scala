package example

import org.scalatest.{FlatSpec, Matchers}

class MyListSpec extends FlatSpec with Matchers {

  "MyList.size" should "return 3 if list has three elements" in {
    MyList(0, 1, 2).size shouldEqual 3
  }

  "MyList.size" should "return o if list is empty" in {
    MyList().size shouldEqual 0
  }

  "MyList.tail" should "return the same list without the first element" in {
    MyList(1,2,3).tail shouldEqual MyList(2,3)
  }

  "MyList.setHead" should "update the first element" in {
    MyList(1, 2, 3).setHead(4) shouldEqual MyList(4, 2, 3)
  }

  "MyList.drop" should "remove N elements" in {
    MyList(1, 2, 3, 4).drop(2) shouldEqual MyList(3, 4)
  }

  "MyList.dropWhile" should "remove as many items as needed" in {
    MyList(7, 6, 5, 4, 3, 2, 1).dropWhile(_ > 3) shouldEqual MyList(3,2,1)
  }

  "MyList.init" should "return the same list without the last element" in {
    MyList(1,2,3,4,5).init() shouldEqual MyList(1, 2, 3, 4)
  }

}
