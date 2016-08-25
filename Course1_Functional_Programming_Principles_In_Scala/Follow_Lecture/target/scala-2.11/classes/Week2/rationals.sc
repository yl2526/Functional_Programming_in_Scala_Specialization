
class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be non-zero")

  def this(x: Int) = this(x, 1) // secondary constructor

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a
    else gcd(b, a % b)
    private val g = gcd(x, y)

  def numer = x / g
  def denom = y / g

  override def toString = numer + "/" + denom

  // + < can be identifier
  def + (that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  // the space between - and : is necessary
  // otherwise -: will be the method name
  def unary_- : Rational = new Rational(-numer, denom)
  def - (that: Rational) = this + -that

  def less(that: Rational) =
    numer * that.denom < that.numer * denom
  def max(that: Rational) = if(this.less(that)) that else that
}


val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)

x.numer
x.denom

x.+(y)
x + y
x.-(y).-(x)

y + y

x.less(y)
x.max(y)

//val strange = new Rational(1,0)

val oneArg = new Rational(2)
