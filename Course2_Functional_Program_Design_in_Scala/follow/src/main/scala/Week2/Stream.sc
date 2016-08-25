import scala.collection.immutable.Stream.Empty

val xs = Stream.cons(1, Stream.cons(2, Stream.empty))

Stream(1,2,3)

(1 to 1000).toStream

def streamRange(lo: Int, hi: Int): Stream[Int] =
  if (lo >= hi) Stream.empty
  else Stream.cons(lo, streamRange(lo + 1, hi))

def listRange(lo: Int, hi: Int): List[Int] =
  if (lo >= hi) Nil
  else lo :: listRange(lo + 1, hi)


1#::2#::Empty

// lazy evaluation

lazy val x = 10 / 3
