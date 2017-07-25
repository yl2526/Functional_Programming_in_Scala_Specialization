package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

import Visualization2._

@RunWith(classOf[JUnitRunner])
class Visualization2Test extends FunSuite with Checkers {

  test("bilinearInterpolation"){
    def interpolate(left: Double, right: Double, pos: Double): Double = {
      (1.0 - pos) * left + pos * right
    }

    assert(bilinearInterpolation(0.5, 0.5, 1, 5, 2, 8) === 4)
    assert(bilinearInterpolation(d00 = 0.0, d01 = -50.0, d10 = 34.926970429418105, d11 = 50.0, x = 0.0, y = 0.1) === -5.0)
  }

}
