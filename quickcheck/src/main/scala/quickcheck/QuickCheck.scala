package quickcheck


import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  private val nonEmptyIntListGen: Gen[List[Int]] = containerOf1[List, Int](arbitrary[Int])
  private val tenIntListGen: Gen[List[Int]] = containerOfN[List, Int](10, arbitrary[Int])

  property("findMin") = forAll(nonEmptyIntListGen) { intList =>
    val h = intList.foldLeft(empty) { (h, x) => insert(x, h)}
    findMin(h) == intList.min
  }

  property("deleteMin") = forAll(tenIntListGen) { intList =>
    val h = intList.foldLeft(empty) { (h, x) => insert(x, h)}
    val h2 = deleteMin(h)
    val diff: List[Int] = intList.diff(List(intList.min))
    findMin(h2) == diff.min
  }

  lazy val genHeap: Gen[H] = { for {
      x <- arbitrary[Int]
      h <- frequency((1, value(empty)), (3, genHeap))
    } yield insert(x, h)
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
