package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import math._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {
    val n: Double = pow(2, zoom)
    val lat = toDegrees(atan(sinh(Pi * (1.0 - 2.0 * y / n))))
    val lon = x.toDouble / n * 360.0 - 180.0
    Location(lat, lon)
  }

  def subtileLocations(zoom: Int, x: Int, y: Int): Seq[Location] = {
    /* Hint: A simple way to achieve that is to rely on the fact that each pixel in a tile
     * can be thought of a subtile at a higher zoom level (256 = 2⁸).
     */
    val subtileLeft = 256 * x
    val subtileTop = 256 * y
    for {
      ySubtile <- subtileTop until subtileTop + 256 // It must iterate y first to be consistent as iterating index
      xSubtile <- subtileLeft until subtileLeft + 256
    } yield tileLocation(zoom + 8, xSubtile, ySubtile)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)],
           zoom: Int, x: Int, y: Int): Image = {
    import Visualization._
    val pixels: Array[Pixel] = subtileLocations(zoom, x, y).par.map({loc =>
      val temp = predictTemperature(temperatures, loc)
      val color = interpolateColor(colors, temp)
      Pixel(color.red, color.green, color.blue, Parameter.alpha)
    }).toArray
    Image(w=256, h=256, pixels)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Int, Data)],
    generateImage: (Int, Int, Int, Int, Data) => Unit
  ): Unit = {
    for {
      tup <- yearlyData
      zoom <- 0 to 3
      x <- 0 until pow(2, zoom).toInt
      y <- 0 until pow(2, zoom).toInt
    } generateImage(tup._1, zoom, x, y, tup._2)
  }

}
