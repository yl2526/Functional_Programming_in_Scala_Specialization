package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.math._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    nodex <- arbitrary[A]
    newHeap <- oneOf(const(empty), genHeap)
  } yield insert(nodex, newHeap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("findMin and deleteMin in order for heap of two") = forAll { (a: A, b: A) =>
    val heapTwo = insert(a, insert(b, empty))
    (min(a, b) == findMin(heapTwo)) && (max(a, b) == findMin(deleteMin(heapTwo))) && isEmpty(deleteMin(deleteMin(heapTwo)))
  }

  property("delete in order for meld fo two heap") = forAll { (ha: H, hb: H) =>
    val hm = meld(ha, hb)
    if (isEmpty(ha) && isEmpty(hb) && isEmpty(hm)) true
    else if (isEmpty(ha) && isEmpty(hb) && (!isEmpty(hm))) false
    else {
      val minhm = findMin(hm)
      (minhm == findMin(ha) || minhm == findMin(hb))
    }
  }

  property("deleteMin and findMin the same node in order") = forAll { la: List[A] =>
    val laSorted = la.sorted

    def listToHeap(remainList: List[A], accuH: H): H = remainList match {
    case Nil => accuH
    case first::rest => listToHeap(rest, insert(first, accuH))
    }
    val h = listToHeap(la, empty)

    def heapToList(remainH: H, accuList: List[A]): List[A] = {
      if (isEmpty(remainH)) accuList
      else {
        heapToList(deleteMin(remainH), findMin(remainH)::accuList)
      }
    }
    val laTransformed = heapToList(h, List()).reverse
    laSorted == laTransformed

  }

}
