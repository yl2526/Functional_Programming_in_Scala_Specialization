
val xs = List(1,2,3)
val ys = List(8, 9)

xs.length

xs.last
xs.init

xs take 2
xs drop 2

xs(2)

xs ++ ys
xs:::ys

xs.reverse

xs updated (2, 15)

xs indexOf 8
xs contains 3

def last[T](xs: List[T]): T = xs match {
  case List() => throw new Error("last of empty list")
  case List(x) => x
  case y :: ys => last(ys)
}

def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init of empty list")
  case List(x) => List()
  case y :: ys => y :: init(ys)
}

def concat[T](xs: List[T], ys: List[T]): List[T]  = xs match {
  case List() => ys
  case z::zs => z::concat(zs, ys)
}

def reverse[T](xs: List[T]): List[T] = xs match {
  case List() => xs
  case y :: ys => reverse(ys) ++ List(y)
}

def removeAt[T](xs: List[T], n: Int) = (xs take n) ::: (xs drop n+1)

def flatten(xs: List[Any]): List[Any] = xs match {
  case List() => xs
  case y :: ys => (y match {
    case aList: List[Any] => flatten(aList)
    case aVal: Any => aVal::Nil
  }) ::: flatten(ys)
}
flatten(List(List(1, 1), 2, List(3, List(5, 8))))


// pair and tuple
val pair = ("answer", 42)
val (label, value) = pair


val label_un = pair._1
val value_un = pair._2






















