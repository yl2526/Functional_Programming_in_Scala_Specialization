


val f: String => String = { case "ping" => "pong" }
f("ping")
//f("pabc")

val f_partial: PartialFunction[String, String] = { case "ping" => "pong" }
f_partial("ping")
f_partial.isDefinedAt("ping")
f_partial.isDefinedAt("pasd")

val f_partial_formal = new PartialFunction[String, String] {
  def apply(x: String) = x match {
    case "ping" => "pong"
  }
  def isDefinedAt(x: String) = x match {
    case "ping" => true
    case _ => false
  }
}
f_partial_formal("ping")
f_partial_formal.isDefinedAt("ping")
f_partial_formal.isDefinedAt("pasd")

val g: PartialFunction[List[Int], String] = {
  case Nil => "one"
  case x :: rest =>
    rest match {
      case Nil => "two"
    }
}

// is DefinedAt only check the first match layer!!!! doesn't guarantee runnable
g.isDefinedAt(List(1,2,3))
g(List(1,2,3))