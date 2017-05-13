package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import java.time.LocalDate
import Extraction._

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {
  val testStationsFile: String = "/station_test.csv"
  val testTemperaturesFile: String = "/1000_test.csv"
  def roundAt5(n: Double): Double = { (math.round(n) * 100000) / 100000 }

  test("read Station to RDD"){
    val read = readStations(testStationsFile).collect().toList
    val result = List((Station("01", ""), Location(10.1, -2.5)), (Station("02", "1"), Location(15, -2)))
    assert(read === result)
  }

  test("read temperatures to RDD"){
    val read = readTemperatures(1000, testTemperaturesFile).collect().toSet
    val result =
      (Station("01", ""), (LocalDate.of(1000, 1, 1), 0.0))::
      (Station("02", "1"), (LocalDate.of(1000, 2, 1), 5.0))::
      (Station("02", "1"), (LocalDate.of(1000, 3, 15), 15.0))::
      Nil
    assert(read === result.toSet)
  }

  test("test join stations and temperatures rdd"){
    val read = locateTemperaturesRDD(1000, testStationsFile, testTemperaturesFile).collect().toSet
    val result =
      (LocalDate.of(1000, 1, 1), Location(10.1, -2.5), 0.0)::
      (LocalDate.of(1000, 2, 1), Location(15, -2), 5.0)::
      (LocalDate.of(1000, 3, 15), Location(15, -2), 15.0)::
      Nil
    assert(read === result.toSet)
  }

  test("test join stations and temperatures seq"){
    val read = locateTemperatures(1000, testStationsFile, testTemperaturesFile)
    val result =
      (LocalDate.of(1000, 1, 1), Location(10.1, -2.5), 0.0)::
        (LocalDate.of(1000, 2, 1), Location(15, -2), 5.0)::
        (LocalDate.of(1000, 3, 15), Location(15, -2), 15.0)::
        Nil
    assert(read.toSet === result.toSet)
  }


  test("yearly temp average"){
    val records = locateTemperatures(1000, testStationsFile, testTemperaturesFile)
    val average = locationYearlyAverageRecords(records)
    val result = List((Location(15, -2), 10.0), (Location(10.1, -2.5), 0.0))
    assert(average.toSet == result.toSet)
  }
  
}