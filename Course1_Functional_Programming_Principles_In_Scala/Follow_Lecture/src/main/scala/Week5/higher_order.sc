val a = List(1,2,3,4,5)

def squareList_pattern(xs: List[Int]): List[Int] = xs match {
  case Nil => Nil
  case y :: ys => y*y :: squareList_pattern(ys)
}
squareList_pattern(a)

def squareList(xs: List[Int]): List[Int] =
  xs map (x => x * x)
squareList(a)

val nums = List(2, -4, 5, 7, 1)
val fruit = List("apple", "pear", "orange", "pineapple")

nums filter (x => x>0)
nums filterNot (x => x>0)
nums partition  (x => x>0)

nums takeWhile (x => x>0)
nums dropWhile (x => x>0)
nums span (x => x>0)


def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (first, rest) = xs span (y => y == x)
    first :: pack(rest)
}
pack(List("a", "a", "a", "b", "c", "c", "a"))

def encode[T](xs: List[T]): List[(T, Int)] =
  pack(xs) map (ys => (ys.head, ys.length))
encode(List("a", "a", "a", "b", "c", "c", "a"))

(0::a) reduceLeft (_+_)
(0::a) reduce (_+_)

(a foldLeft 0)(_+_)
(a fold 0)(_+_)
a.fold(0)(_+_)

(0::a) reduceRight (_+_)
(a foldRight 0)(_+_)

def concat[T](xs: List[T], ys: List[T]): List[T] =
  // foldLeft causes type error
  (xs foldRight ys) (_ :: _)