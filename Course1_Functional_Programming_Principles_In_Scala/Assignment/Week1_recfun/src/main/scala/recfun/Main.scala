package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int =
    if (c < 0 || c > r) 0
    else if (r <= 1) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
    * Exercise 2
    */
 def balance(chars: List[Char]): Boolean = {
    def balanced(chars: List[Char], openCount: Int): Boolean = {
      if (openCount < 0) false
      else if (chars.isEmpty) if (openCount == 0) true else false
      else if (chars.head == '(') balanced(chars.tail, openCount + 1)
      else if (chars.head == ')') balanced(chars.tail, openCount - 1)
      else balanced(chars.tail, openCount)
    }
    balanced(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def methods(left: Int, coins_use: List[Int]): Int = {
      if (left == 0)
        1
      else if (left < 0)
        0
      else if ((left > 0) && (coins_use.isEmpty))
        0
      else
        methods(left, coins_use.tail) + methods(left - coins_use.head, coins_use)
    }
    methods(money, coins)
  }
}
