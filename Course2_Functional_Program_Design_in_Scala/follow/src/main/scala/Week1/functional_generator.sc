trait Generator[+T] {
  self =>
  // an alias for "this".
  // this will make the self.generate call the generate of the trait instead of the map
  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    def generate = f(self.generate) // or use Generate.this.generate
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    def generate = f(self.generate).generate
  }

}

val integers = new Generator[Int] {
  val rand = new java.util.Random
  def generate = rand.nextInt()
}

val booleans = for (x <- integers) yield x > 0
// or val booleans = integers map { x => x > 0 }
 /*
  *expanded to
  *val booleans = new Generator[Boolean] {
  *  def generate = (x: Int => x > 0)(integers.generate)
  *}
  */

def pairs[T, U](t: Generator[T], u: Generator[U]) = t flatMap {
  x => u map { y => (x, y) } }

def single[T](x: T): Generator[T] = new Generator[T] {
  def generate = x
}

def choose(lo: Int, hi: Int): Generator[Int] =
  for (x <- integers) yield lo + x % (hi - lo)

def oneOf[T](xs: T*): Generator[T] =
  for (idx <- choose(0, xs.length)) yield xs(idx)

def lists: Generator[List[Int]] = for {
  isEmpty <- booleans
  list <- if (isEmpty) emptyLists else nonEmptyLists
} yield list

def emptyLists = single(Nil)
def nonEmptyLists = for {
  head <- integers
  tail <- lists
} yield head :: tail


// Random Test Function
// This is so important, First time I know how to do this...
def test[T](g: Generator[T], numTimes: Int = 100)
           (test: T => Boolean): Unit = {
  for (i <- 0 until numTimes) {
    val value = g.generate
    assert(test(value), "test failed for "+value)
  }
  println("passed "+numTimes+ " tests")
}

test(pairs(lists, lists)) {
  case (xs, ys) => (xs ++ ys).length > xs.length
}

// ScalaCheck will do this
// https://www.scalacheck.org/
/* do something like this
forAll { (l1: List[Int], l2: List[Int]) =>
  l1.size + l2.size == (l1 ++ l2).size
}
*/


