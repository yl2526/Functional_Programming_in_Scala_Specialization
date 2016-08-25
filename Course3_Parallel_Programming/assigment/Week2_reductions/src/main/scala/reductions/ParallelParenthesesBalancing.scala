package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def balanced(chars: Array[Char], openCount: Int): Boolean = {
      if (openCount < 0) false
      else if (chars.isEmpty) openCount == 0
      else if (chars.head == '(') balanced(chars.tail, openCount + 1)
      else if (chars.head == ')') balanced(chars.tail, openCount - 1)
      else balanced(chars.tail, openCount)
    }
    balanced(chars, 0)
  }


  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, leftNeeded: Int, rightNeeded: Int): (Int, Int) = {
      if (until == idx) (leftNeeded, rightNeeded)
      else {
        //leftNeeded, rightNeeded number of ( in left or ) in right needed to balance
        val currentChar = chars(idx)
        if (currentChar == '(')
          traverse(idx + 1, until, leftNeeded, rightNeeded + 1)
        else if (currentChar == ')')
          if (rightNeeded > 0 ) traverse(idx + 1, until, leftNeeded, rightNeeded - 1)
          else traverse(idx + 1, until, leftNeeded + 1, rightNeeded)
        else traverse(idx + 1, until, leftNeeded, rightNeeded)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold)
        traverse(from, until, 0, 0)

      else {
        val (left: (Int, Int), right: (Int, Int)) = parallel(reduce(from, (from+until)/2), reduce((from+until)/2, until))
        if (left._2 <= right._1) (left._1 + right._1 - left._2, right._2)
        else (left._1, right._2 + left._2 - right._1)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

}
