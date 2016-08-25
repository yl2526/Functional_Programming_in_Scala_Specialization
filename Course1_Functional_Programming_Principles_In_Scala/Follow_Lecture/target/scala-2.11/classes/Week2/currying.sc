/*
def product(f: Int => Int)(a: Int , b: Int): Int = {
  if (a > b) 1
  else f(a) * product(f)(a+1, b)
}

product(x => x * x)(1,2)
product(x => x)(1, 3)
*/
def fact(n: Int): Int = product(x => x)(1, n)

fact(3)

def mapReduce(map: Int => Int, reduce: (Int, Int) => Int, identity: Int)(a: Int , b: Int): Int = {
  if (a > b) identity
  else reduce(map(a), reduce(a+1, b))
}

def product(f: Int => Int)(a: Int , b: Int) =
  mapReduce(f, (x, y) => x * y, 1)(a, b)

product(x => x * x)(1,2)
product(x => x)(1, 3)


// Fixed Points
import math.abs

val tolerance = 0.0001
def isCloseEnough(x: Double, y: Double) =
  abs((x - y) / x) / x < tolerance
def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  def iterate(guess: Double): Double = {
    val next = f(guess)
    if (isCloseEnough(guess, next)) next
    else iterate(next)
  }
  iterate(firstGuess)
}

/*
def sqrt(x: Double) =
  fixedPoint(y => (y + x / y) / 2)(1.0)

sqrt(2)
*/
def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

def sqrt(x: Double) =
  fixedPoint(averageDamp(y => x / y))(1.0)

sqrt(2)