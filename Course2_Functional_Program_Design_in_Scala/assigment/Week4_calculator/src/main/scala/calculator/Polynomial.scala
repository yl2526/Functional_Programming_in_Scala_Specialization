package calculator
import scala.math.{pow, sqrt}

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Var(pow(b(), 2) - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Var{
      if (delta() < 0) Set.empty[Double]
      else Set( (-b() + sqrt(delta())) / 2.0 / a(), (-b() - sqrt(delta())) / 2.0 / a() )
    }
    // just realize that set will automatically take care of the same roots. No need for else if.
  }

  /*
  This change is to deal with the test below.

  [Test Description] computeSolutions
  [Observed Error] 2 was not equal to 0
  [Lost Points] 3

  I search about this and found a discussion. People there mentions this might due to cyclic reference.
    [https://coderanch.com/t/648916/Scala/Reactive-Programming-Week-Assignment-Calculator]
  Then, my brain blow up. Like how???
  Then I realize if I do it in the old way below, I end up with two signals. But how can this related to cyclic reference???
   */

  /*
  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    if (delta() < 0) Var(Set.empty[Double])
    else if (delta() == 0.0) Var(Set( -b() / 2.0 / a() ))
    else Var(Set( (-b() + sqrt(delta())) / 2.0 / a(), (-b() - sqrt(delta())) / 2.0 / a() ))
  }
  */
}