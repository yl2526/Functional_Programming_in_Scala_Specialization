val nums = List(2, -4, 5, 7, 1)
val fruit = List("apple", "pear", "orange", "pineapple")

// using lt func parameter
def msort_lt[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
  val n = xs.length/2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (Nil, ys) => ys
      case(xs, Nil) => xs
      case(x::xs1, y::ys1) => {
        if (lt(x, y)) x::merge(xs1, ys)
        else y::merge(xs, ys1)
      }
    }
    val (fst, snd) = xs splitAt n
    merge(msort_lt(fst)(lt), msort_lt(snd)(lt))
  }
}

msort_lt(nums)((x, y) => x < y)
msort_lt(fruit)((x: String, y: String) => x.compareTo(y) < 0)


// using ordering class
import math.Ordering
def msort_order[T](xs: List[T])(ord: Ordering[T]): List[T] = {
  val n = xs.length/2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (Nil, ys) => ys
      case(xs, Nil) => xs
      case(x::xs1, y::ys1) => {
        if (ord.lt(x, y)) x::merge(xs1, ys)
        else y::merge(xs, ys1)
      }
    }
    val (fst, snd) = xs splitAt n
    merge(msort_order(fst)(ord), msort_order(snd)(ord))
  }
}

msort_order(nums)(Ordering.Int)
msort_order(fruit)(Ordering.String)


import math.Ordering
def msort_implicit[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length/2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (Nil, ys) => ys
      case(xs, Nil) => xs
      case(x::xs1, y::ys1) => {
        if (ord.lt(x, y)) x::merge(xs1, ys)
        else y::merge(xs, ys1)
      }
    }
    val (fst, snd) = xs splitAt n
    // the second ord func arg is no long necessary
    merge(msort_implicit(fst), msort_implicit(snd))
  }
}

msort_implicit(nums)
msort_implicit(fruit)
