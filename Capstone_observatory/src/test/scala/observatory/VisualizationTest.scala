package observatory


import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

import math._
import Visualization._


@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {

  def roundAt10(dist: Double) = round(dist / 10) * 10

  test("great circle distance"){
    assert(roundAt10(greatCircleDistance(Location(10, 10), Location(10, 10))) === 0)
    assert(roundAt10(greatCircleDistance(Location(90, 0), Location(-90, 0))) === roundAt10(Pi * 6371))
    assert(roundAt10(greatCircleDistance(Location(10, 50), Location(-50, 70))) === 6950)
  }

  test("predict temperature at close place"){
    val temps = List((Location(10, 10), 1.0), (Location(90, 70), 50.0))
    val result = predictTemperature(temps, Location(10.005, 9.995))
    assert(result === 1.0)
  }

  test("predict temperature at far place"){
    val temps = List((Location(10, 10), 0.0), (Location(30, 30), 2.0))
    val result = predictTemperature(temps, Location(20, 20))
    assert(round(result) === 1)
  }

  test("choice two node"){
    val colorTemps = List((5.0, Color(1,2,3)), (15.0, Color(3,2,1)), (20.0, Color(5,5,5)))
    val result = ((5.0, Color(1,2,3)), (15.0, Color(3,2,1)))
    assert(choiceTwoPoints(colorTemps, 6) === result)
  }

  test("interpolate two color"){
    assert(interpolateTwoColor((5.0, Color(1,2,3)), (15.0, Color(3,2,1)), 10) === Color(2,2,2))
  }

  test("interpolate color in the list"){
    assert(interpolateTwoColor((5.0, Color(1,2,3)), (15.0, Color(3,2,1)), 5.0) === Color(1,2,3))
  }

  test("interpolate two color outside range"){
    assert(interpolateTwoColor((5.0, Color(1,2,3)), (15.0, Color(3,2,1)), 1) === Color(1,2,3))
  }

  test("interpolate color"){
    val colorTemps = List((5.0, Color(1,2,3)), (15.0, Color(3,2,1)), (20.0, Color(5,5,5)))
    assert(interpolateColor(colorTemps, 10.0) === Color(2,2,2))
  }

  test("interpolate color outside range"){
    val colorTemps = List((15.0, Color(3,2,1)), (5.0, Color(1,2,3)))
    assert(interpolateColor(colorTemps, 20.0) === Color(3,2,1))
  }

  test("index to location"){
    assert(indexToLocation(0) === Location(90, -180))
    assert(indexToLocation(359) === Location(90, 179))
    assert(indexToLocation(360) === Location(89, -180))
    assert(indexToLocation(720-1) === Location(89, 179))
    assert(indexToLocation(90*360 + 180) === Location(0, 0))
  }

  ignore("try make a image"){
    import Extraction._
    val temperatures = locationYearlyAverageRecords(locateTemperatures(2000, "/stations.csv", "/2000.csv"))
    val picture = new java.io.File("C:\\Users\\chaor\\Desktop\\git repos\\Functional_Programming_in_Scala_Specialization\\Capstone_observatory\\src\\main\\target\\raw_data_vis.png")
    val _ = visualize(temperatures, Parameter.colorBar).output(picture)
  }

}
