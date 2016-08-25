trait Expr
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
/*
implicitly doing this
object Number {
  def apply(n: Int) = new Number(n)
}
object Sum {
  def apply(e1: Expr, e2: Expr) = new Sum(e1, e2)
}
 */

def eval(e: Expr): Int = e match {
  case Number(n) => n
  case Sum(e1, e2) => eval(e1) + eval(e2)
}

eval(Sum(Number(1), Number(2)))




//insertion sort
def isort(xs: List[Int]): List[Int] = xs match {
  case List() => List()
  case y :: ys => insert(y, isort(ys))
}

def insert(x: Int, xs: List[Int]): List[Int] =
  xs match {
    case List() => List(x)
    case y :: ys => if (x <= y) x::xs else y::insert(x, ys)
  }

isort(List(3, 5, 1, 9, 7))

