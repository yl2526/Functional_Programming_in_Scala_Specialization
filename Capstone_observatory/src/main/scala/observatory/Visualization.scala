package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import math._

/**
  * 2nd milestone: basic visualization
  */
object Visualization {
  def greatCircleDistance(a: Location, b: Location): Double = {
    val r = 6371
    val delta = sin(toRadians(a.lat)) * sin(toRadians(b.lat)) +
      cos(toRadians(a.lat)) * cos(toRadians(b.lat)) * cos(toRadians(abs(a.lon - b.lon)))
    r * acos(max(min(delta, 1), -1))
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    val distanceTemperatures = temperatures.par.map{case (loc, temp) => (greatCircleDistance(loc, location), temp)}
    val cloestDistance = distanceTemperatures.minBy(_._1)
    if (cloestDistance._1 <= 1) cloestDistance._2
    else {
      val result = distanceTemperatures.foldLeft((0.0, 0.0))((accu, pair) => {
        val weight = pow(pair._1, - Parameter.interpolationPower)
        (accu._1 + weight*pair._2, accu._2 + weight)
      })
      result._1 / result._2
    }
  }

  def choiceTwoPoints(points: Iterable[(Double, Color)], temperature: Double): ((Double, Color), (Double, Color)) = {
    val pointsDiff = points.map(point => (abs(point._1 - temperature), point)).toList.sortBy(_._1)
    (pointsDiff(0)._2, pointsDiff(1)._2)
  }

  def interpolateTwoColor(left: (Double, Color), right: (Double, Color), temperature: Double): Color = {
    if (temperature <= left._1) left._2
    else if (temperature >= right._1) right._2
    else {
      val leftPara: Double = 1.0 - (temperature - left._1) / (right._1 - left._1)
      val rightPara: Double = 1.0 - (right._1 - temperature) / (right._1 - left._1)
      def interpolate(leftColor: Int, rightColor: Int): Int = round(leftColor * leftPara + rightColor * rightPara).toInt
      Color(
        interpolate(left._2.red, right._2.red),
        interpolate(left._2.green, right._2.green),
        interpolate(left._2.blue, right._2.blue)
      )
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    val twoColor = choiceTwoPoints(points, value)
    if (twoColor._1._1 <= twoColor._2._1) interpolateTwoColor(twoColor._1, twoColor._2, value)
    else interpolateTwoColor(twoColor._2, twoColor._1, value)
  }

  def indexToLocation(index: Int): Location = {
    val lat: Double = 90 - index / 360
    val lon: Double = index % 360 - 180
    Location(lat, lon)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    val pixels: Array[Pixel] = (0 until 360*180).par.map({index =>
      val temp = predictTemperature(temperatures, indexToLocation(index))
      val color = interpolateColor(colors, temp)
      Pixel(color.red, color.green, color.blue, Parameter.alpha)
    }).toArray
    Image(w=360, h=180, pixels)
  }

}

