package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import math._

import Interaction.{tileLocation, subtileLocations}
import Visualization.interpolateColor

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param x X coordinate between 0 and 1
    * @param y Y coordinate between 0 and 1
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    x: Double,
    y: Double,
    d00: Double,
    d01: Double,
    d10: Double,
    d11: Double
  ): Double = {
    def interpolate(left: Double, right: Double, pos: Double): Double = {
      (1.0 - pos) * left + pos * right
    }
    interpolate(interpolate(d00, d10, x), interpolate(d01, d11, x), y)
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param zoom Zoom level of the tile to visualize
    * @param x X value of the tile to visualize
    * @param y Y value of the tile to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: (Int, Int) => Double,
    colors: Iterable[(Double, Color)],
    zoom: Int,
    x: Int,
    y: Int
  ): Image = {
    val tile = tileLocation(zoom, x, y)
    val n: Double = pow(2, zoom)
    val xRange = 360.0 / n
    val yRange = 180.0 / n

    val pixels: Array[Pixel] = subtileLocations(zoom, x, y).par.map({loc =>
      val lat = ceil(loc.lat).toInt // the point is at top left
      val lon = loc.lon.toInt
      val temp = bilinearInterpolation(
        loc.lon - lon,
        lat - loc.lat,
        grid(lat, lon),
        grid(lat - 1, lon), // the anchor point is at top
        grid(lat, lon + 1),
        grid(lat - 1, lon + 1))
      val color = interpolateColor(colors, temp)

      Pixel(color.red, color.green, color.blue, Parameter.alpha)
    }).toArray
    Image(w=256, h=256, pixels)
  }

}
