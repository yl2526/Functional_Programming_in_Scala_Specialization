package observatory

/**
  * paramete object like the power for interpoltiion and colors...
  */
object Parameter {
  val interpolationPower: Int = 2
  val alpha: Int = 125
  val colorBar: Seq[(Double, Color)] = Seq(
    (60, Color(red = 255, green = 255, blue = 255)),
    (32, Color(red = 255, green = 0, blue = 0)),
    (12, Color(red = 255, green = 255, blue = 0)),
    (0, Color(red = 0, green = 255, blue = 255)),
    (-15, Color(red = 0, green = 0, blue = 255)),
    (-27, Color(red = 255, green = 0, blue = 255)),
    (-50, Color(red = 33, green = 0, blue = 107)),
    (-60, Color(red = 0, green = 0, blue = 0))
  )

}
