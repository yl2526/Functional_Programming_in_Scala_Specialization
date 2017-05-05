package observatory

import java.time.LocalDate
import org.apache.commons.io.FilenameUtils
import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import scala.reflect.ClassTag

import org.apache.log4j.{Logger, Level}

/**
  * 1st milestone: data extraction
  */
object Extraction {

  Logger.getLogger("org").setLevel(Level.OFF)
  Logger.getLogger("akka").setLevel(Level.OFF)
  @transient lazy val conf: SparkConf = new SparkConf().setMaster("local").setAppName("capstone")
  @transient lazy val sc: SparkContext = new SparkContext(conf)

  def urlToPath(url: String): String = {
    // I didn't realize the absolute path was talking about this url thing...
    if (System.getProperty("os.name").contains("Windows"))
      "C:\\Users\\chaor\\Desktop\\git repos\\Functional_Programming_in_Scala_Specialization\\Capstone_observatory\\src\\main\\resources\\" + url.tail
    else getClass.getResource(url).getPath
  }

  /**
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @return mapping from Station -> Location
    */
  def readStations(stationsFile: String): RDD[(Station, Location)] = {
    val stationLines = sc.textFile(urlToPath(stationsFile))
    stationLines
      .map(_.split(","))
      .filter(lineArr => (lineArr.length == 4) && (lineArr(2) != "") && (lineArr(3) != ""))
      .map(lineArr => (Station(STN = lineArr(0), WBAN = lineArr(1)),
                       Location(lat = lineArr(2).toDouble, lon = lineArr(3).toDouble)))
  }

  /**
    * @param temperaturesFile     Path of the stations resource file to use (e.g. "/2017.csv")
    * @return mapping from Station -> (LocalDate, Double)
    */
  def readTemperatures(year: Int, temperaturesFile: String): RDD[(Station, (LocalDate, Double))] = {
    val temperaturesLines = sc.textFile(urlToPath(temperaturesFile))
    def toC(f: String): Double = (f.toDouble - 32) * 5.0 / 9.0
    temperaturesLines
      .map(_.split(","))
      .filter(lineArr => (lineArr.length == 5) && (lineArr(2) != "") && (lineArr(3) != ""))
      .map(lineArr => (Station(STN = lineArr(0), WBAN = lineArr(1)),
        (LocalDate.of(year, lineArr(2).toInt, lineArr(3).toInt), toC(lineArr(4)))))
  }

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A RDD containing triplets (date, location, temperature)
    */
  def locateTemperaturesRDD(year: Int, stationsFile: String, temperaturesFile: String): RDD[(LocalDate, Location, Double)] = {
    val stations = readStations(stationsFile)
    val temperatures = readTemperatures(year, temperaturesFile)
    temperatures.join(stations).map{case (_, ((date, temp), loc)) => (date, loc, temp)}
  }

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    locateTemperaturesRDD(year, stationsFile, temperaturesFile).collect.toSeq
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    records
      .groupBy(_._2)
      .mapValues{ records =>
        val pairs = (records foldLeft (0.0, 0))((accu, record) => (accu._1 + record._3, accu._2 + 1))
        pairs._1 / pairs._2.toDouble
      }
  }

}
