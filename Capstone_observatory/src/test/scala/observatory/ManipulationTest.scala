package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

import Manipulation._
import Visualization._

@RunWith(classOf[JUnitRunner])
class ManipulationTest extends FunSuite with Checkers {

  test("make grid"){
    val temps = List((Location(10, 10), 10.0), (Location(20, 20), 2.0))
    val grid = makeGrid(temps)
    assert(grid(15, 15) === predictTemperature(temps, Location(15, 15)))
    assert(grid(11, 17) === predictTemperature(temps, Location(11, 17)))
  }

  test("average"){
    val temps1 = List((Location(10, 10), 10.0), (Location(20, 20), 2.0))
    val temps2 = List((Location(50, 10), 12.0), (Location(20, 6), 30.0))
    val avg = average(temps1::temps2::Nil)
    assert(avg(15, 15) === (predictTemperature(temps1, Location(15, 15)) +
                            predictTemperature(temps2, Location(15, 15))
                            ) / 2)
    assert(avg(30, 25) === (predictTemperature(temps1, Location(30, 25)) +
      predictTemperature(temps2, Location(30, 25))
      ) / 2)
  }

}