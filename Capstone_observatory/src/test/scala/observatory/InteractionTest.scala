package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

import scala.collection.concurrent.TrieMap

import math._
import Interaction._

@RunWith(classOf[JUnitRunner])
class InteractionTest extends FunSuite with Checkers {
  def roundAf(dist: Double): Double = round(dist * 10000.0) / 10000.0

  test("tile location at 0 0 "){
    val result = tileLocation(0, 0, 0)
    assert(roundAf(result.lat) === 85.0511)
    assert(roundAf(result.lon) === -180.0)
  }

  ignore("create some tile image"){
    import Extraction._
    val temperatures = locationYearlyAverageRecords(locateTemperatures(2000, "/stations.csv", "/2000.csv"))
    val picture = new java.io.File("C:\\Users\\chaor\\Desktop\\git repos\\Functional_Programming_in_Scala_Specialization\\Capstone_observatory\\src\\main\\target\\raw_data_vis.png")
    val _ = tile(temperatures, Parameter.colorBar, 1, 0, 0).output(picture)
  }

}
