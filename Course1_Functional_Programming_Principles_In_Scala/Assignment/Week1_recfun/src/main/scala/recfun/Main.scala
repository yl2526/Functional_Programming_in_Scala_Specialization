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
      else if (chars.isEmpty) openCount == 0
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
    if (money == 0)
      1
    else if (money < 0)
      0
    else if ((money > 0) && (coins.isEmpty))
      0
    else
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
