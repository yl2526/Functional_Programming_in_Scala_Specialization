import quickcheck.Bogus4BinomialHeap

object Intb4 extends Bogus4BinomialHeap {
  override type A = Int
  override def ord = scala.math.Ordering.Int
}
import Intb4._

val h1 = insert(3, insert(1, insert(2, empty)))

findMin(h1)
deleteMin(h1)

findMin(deleteMin(h1))
deleteMin(deleteMin(h1))

val a = List(1,2,3)
a.length