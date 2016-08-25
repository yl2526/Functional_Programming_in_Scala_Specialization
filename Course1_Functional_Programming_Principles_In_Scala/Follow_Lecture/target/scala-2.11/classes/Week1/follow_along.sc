1+2
def abs(x: Double) = if (x < 0) -x else x
/*
def sqrtIter(guess: Double, x: Double): Double =
  if (isGoodEnough(guess, x)) guess
  else sqrtIter(improve(guess, x), x)

def isGoodEnough(guess: Double, x: Double): Boolean =
  abs(guess * guess - x) / x < 0.001

def improve(guess: Double, x: Double) =
  (guess + x / guess) / 2

def sqrt(x: Double) = sqrtIter(1.0, x)
*/

// to restrict access and avoid name space pollution
// delcare another x to reduce occurrence of x

def sqrt(x: Double) = {
  def sqrtIter(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))

  def isGoodEnough(guess: Double): Boolean =
    abs(guess * guess - x) / x < 0.001

  def improve(guess: Double) =
    (guess + x / guess) / 2

  sqrtIter(1.0)
}

sqrt(0.001)
sqrt(0.1e-20)
sqrt(1.0e20)
sqrt(1.0e50)


// break a line into multiple line space
(1 +
  2)
// or just put operator at the end of first line
1+
  2

/*
  rewriting
*/

def gcd(a: Int, b: Int): Int =
  if (b == 0) a else gcd(b, a % b)

gcd(14, 21)

/*
def factorial(n: Int): Int =
  if (n == 0) 1 else n * factorial(n - 1)
 */
def factorial(n: Int): Int = {
  def loop(acc: Int, n: Int): Int =
    if(n == 0) acc
    else loop(acc *n, n - 1)

  loop(1, n)
}

factorial(4)
// tail rule!!! if last action  is calling a function