package calculator
import scala.math.{pow, sqrt}

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    val delta = Var(0.0)
    delta() = pow(b(), 2) - 4 * a() * c()
    delta
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    if (delta() < 0) Var(Set.empty[Double])
    else if (delta() == 0.0) Var(Set(-b()/2/a()))
    else Var(Set((-b()+sqrt(delta()))/2/a(), (-b()-sqrt(delta()))/2/a()))
  }
}
