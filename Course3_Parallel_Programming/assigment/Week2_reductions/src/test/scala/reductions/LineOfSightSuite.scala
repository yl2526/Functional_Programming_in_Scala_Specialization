package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory

@RunWith(classOf[JUnitRunner]) 
class LineOfSightSuite extends FunSuite {
  import LineOfSight._

  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assert(output.toList === List(0f, 1f, 4f, 4f))
  }


  test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val res = upsweepSequential(Array[Float](0f, 1f, 8f, 9f), 1, 4)
    assert(res == 4f)
  }


  test("downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](4)
    downsweepSequential(Array[Float](0f, 1f, 8f, 9f), output, 0f, 1, 4)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("upsweepSequential") {
    val input = Array[Float](0f, 1f, 8f, 9f)
    val output = new Array[Float](4)
    assert(upsweepSequential(input, 0 , 1) === 0)
  }

  test("downsweepSequential") {
    val input = Array[Float](0f, 1f, 8f, 9f, 12f, 25f, 16f, 49f)
    val output = new Array[Float](8)
    val result = List(0f, 1f, 4f, 4f, 4f, 5f, 5f, 7f)

    downsweepSequential(input, output, 0f, 0 , 1)
    assert(output(0) === 0)

    downsweepSequential(input, output, 0f, 1 , 3)
    assert(output(1) === 1f)
    assert(output(2) === 4f)

    downsweepSequential(input, output, 5f,6 , 8)
    assert(output(6) === 5f)
    assert(output(7) === 7f)

  }

  test("parLineOfSight") {
    val input = Array[Float](0f, 8f, 14f, 33f, 48f)
    val output = new Array[Float](5)
    val result = List(0.0, 8.0, 8.0, 11.0, 12.0)
    val threshold = 1

    val previousMaxTree = upsweep(input, 0, input.length, threshold)

    downsweep(input, output, 1, previousMaxTree)
    println(output.toList)
    assert(output.toList === result)
  }

  test("parLineOfSight longer") {
    val input = Array[Float](0f, 1f, 8f, 9f, 12f, 25f, 16f, 49f)
    val output = new Array[Float](8)
    val result = List(0f, 1f, 4f, 4f, 4f, 5f, 5f, 7f)
    val threshold = 1

    val previousMaxTree = upsweep(input, 0, input.length, threshold)

    downsweep(input, output, 0, previousMaxTree)

    assert(output.toList === result)
  }


}

