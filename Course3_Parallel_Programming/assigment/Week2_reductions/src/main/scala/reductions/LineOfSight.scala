package reductions

import org.scalameter._
import common._

object LineOfSightRunner {
  
  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 100,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]) {
    val length = 10000000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output = new Array[Float](length + 1)
    val seqtime = standardConfig measure {
      LineOfSight.lineOfSight(input, output)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      LineOfSight.parLineOfSight(input, output, 10000)
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}

object LineOfSight {

  def max(a: Float, b: Float): Float = if (a > b) a else b

  def lineOfSight(input: Array[Float], output: Array[Float]): Unit = {
    var poi = 1
    var previousMax: Float = 0f
    output(0) = 0
    while (poi < input.length) {
      val currentTan: Float = input(poi) / poi
      if (currentTan <= previousMax)
        output(poi) = previousMax
      else {
        output(poi) = currentTan
        previousMax = currentTan
      }
      poi += 1
    }

  }

  sealed abstract class Tree {
    def maxPrevious: Float
  }

  case class Node(left: Tree, right: Tree) extends Tree {
    val maxPrevious = max(left.maxPrevious, right.maxPrevious)
  }

  case class Leaf(from: Int, until: Int, maxPrevious: Float) extends Tree

  /** Traverses the specified part of the array and returns the maximum angle.
   */
  def upsweepSequential(input: Array[Float], from: Int, until: Int): Float = {
    var poi = from
    var previousMax: Float = 0f
    while (poi < until) {
      if (poi != 0)
        previousMax = max(previousMax, input(poi) / poi)
      poi += 1
    }
    previousMax
  }

  /** Traverses the part of the array starting at `from` and until `end`, and
   *  returns the reduction tree for that part of the array.
   *
   *  The reduction tree is a `Leaf` if the length of the specified part of the
   *  array is smaller or equal to `threshold`, and a `Node` otherwise.
   *  If the specified part of the array is longer than `threshold`, then the
   *  work is divided and done recursively in parallel.
   */
  def upsweep(input: Array[Float], from: Int, end: Int,
    threshold: Int): Tree = {
    if (end - from <= threshold)
      Leaf(from, end, upsweepSequential(input, from, end))
      /*
     Two test suggest me to use the parallel(a,b,c,d) constructor but if I do that, other test will calim I call task
     don't know what to do
    else if (end - from >= threshold * 3 && end - from <= threshold * 4) {
      val (ll, lr, rl, rr) = parallel(
        upsweep(input, from, from + threshold, threshold),
        upsweep(input, from + threshold, from + threshold * 2, threshold),
        upsweep(input, from + threshold * 2, from + threshold * 3, threshold),
        upsweep(input, from + threshold * 3, end, threshold)
      )
      Node(Node(ll, lr), Node(rl, rr))
    }
    */
    else {
      val mid = (from + end) / 2
      val (leftTree, rightTree) = parallel(upsweep(input, from, mid, threshold),
                                           upsweep(input, mid, end, threshold))
      Node(leftTree, rightTree)
    }
  }

  /** Traverses the part of the `input` array starting at `from` and until
   *  `until`, and computes the maximum angle for each entry of the output array,
   *  given the `startingAngle`.
   */
  def downsweepSequential(input: Array[Float], output: Array[Float],
    startingAngle: Float, from: Int, until: Int): Unit = {
    if (from == 0) {
      output(0) = 0
      downsweepSequential(input, output, startingAngle, from + 1, until)
    }
    else {
      var poi = from
      var previousMax: Float = startingAngle
      while (poi < until) {
        val currentTan: Float = input(poi) / poi
        if (currentTan <= previousMax)
          output(poi) = previousMax
        else {
          output(poi) = currentTan
          previousMax = currentTan
        }
        poi += 1
      }
    }
  }

  /** Pushes the maximum angle in the prefix of the array to each leaf of the
   *  reduction `tree` in parallel, and then calls `downsweepTraverse` to write
   *  the `output` angles.
   */
  def downsweep(input: Array[Float], output: Array[Float], startingAngle: Float,
    tree: Tree): Unit = tree match {
    case Node(lTree, rTree) => {
      parallel(downsweep(input, output, startingAngle, lTree),
               downsweep(input, output, max(startingAngle, lTree.maxPrevious), rTree))
    }
    case Leaf(from, end, _) =>
      downsweepSequential(input, output, startingAngle, from, end)
  }

  /** Compute the line-of-sight in parallel. */
  def parLineOfSight(input: Array[Float], output: Array[Float],
    threshold: Int): Unit = {
    val previousMaxTree = upsweep(input, 0, input.length, threshold)
    downsweep(input, output, 0, previousMaxTree)
  }
}
