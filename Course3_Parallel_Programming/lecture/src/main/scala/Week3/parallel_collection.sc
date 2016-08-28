import scala.collection._

def largestPalindrome(xs: GenSeq[Int]): Int = {
  xs.aggregate(Int.MinValue)(
    (largest, n) =>
      if (n > largest && n.toString == n.toString.reverse) n else largest,
    math.max
  )
}

val array = (0 until 1000000).toArray

largestPalindrome(array)
largestPalindrome(array.par)



def intersection(a: GenSet[Int], b: GenSet[Int]): Set[Int] = {
  val result = mutable.Set[Int]() // scala mutable set is not thread safe, may be corrupt
  for (x <- a) if (b contains x) result += x
  result
}
val seqRes = intersection((0 until 1000).toSet, (0 until 1000 by 4).toSet)
val parRes = intersection((0 until 1000).par.toSet, (0 until 1000 by 4).par.toSet)

seqRes == parRes
seqRes.size
parRes.size

import java.util.concurrent._
def intersection2(a: GenSet[Int], b: GenSet[Int]) = {
  val result = new ConcurrentSkipListSet[Int]()
  for (x <- a) if (b contains x) result.add(x)
  result
}
val seqRes2 = intersection2((0 until 1000).toSet, (0 until 1000 by 4).toSet)
val parRes2 = intersection2((0 until 1000).par.toSet, (0 until 1000 by 4).par.toSet)

seqRes2 == parRes2
seqRes2.size
parRes2.size

def intersection3(a: GenSet[Int], b: GenSet[Int]): GenSet[Int] = {
  if (a.size < b.size) a.filter(b(_))
  else b.filter(a(_))
}
val seqRes3 = intersection3((0 until 1000).toSet, (0 until 1000 by 4).toSet)
val parRes3 = intersection3((0 until 1000).par.toSet, (0 until 1000 by 4).par.toSet)

seqRes3 == parRes3
seqRes3.size
parRes3.size

// Never write to a collection that is concurrently traversed.
// Never read from a collection that is concurrently modified.
// program non-deterministically prints different results, or crashes.

val graph = concurrent.TrieMap[Int, Int]() ++= (0 until 100000).map(i => (i, i + 1))
graph(graph.size - 1) = 0
val previous = graph.snapshot()
for ((k, v) <- graph.par) graph(k) = previous(v)
val violation = graph.find({ case (i, v) => v != (i + 2) % graph.size })
println(s"violation: $violation")
