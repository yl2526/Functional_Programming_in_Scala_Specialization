import Week3._

object sim extends Circuits with Parameters
import sim._
val input1, input2, sum, carry = new Wire

halfAdder(input1, input2, sum, carry)
probe("sum", sum)
probe("carry", carry)

input1.setSignal(true)
run()

input2.setSignal(true)
run()