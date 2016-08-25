package reductions

import org.scalameter._
import common._

object ParallelCountChangeRunner {

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 80,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val amount = 250
    val coins = List(1, 2, 5, 10, 20, 50)
    val seqtime = standardConfig measure {
      seqResult = ParallelCountChange.countChange(amount, coins)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime ms")

    def measureParallelCountChange(threshold: ParallelCountChange.Threshold): Unit = {
      val fjtime = standardConfig measure {
        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
      }
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime ms")
      println(s"speedup: ${seqtime / fjtime}")
    }

    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))
  }
}

object ParallelCountChange {

  /** Returns the number of ways change can be made from the specified list of
   *  coins for the specified amount of money.
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def numChangeWay(left: Int, changeTrace: List[Int]): Int = {
      if (left == 0)
        1
      else if (left < 0)
        0
      else if ((left > 0) && changeTrace.isEmpty)
        0
      else
        numChangeWay(left, changeTrace.tail) + numChangeWay(left - changeTrace.head, changeTrace)
    }
    numChangeWay(money, coins)
  }

  type Threshold = (Int, List[Int]) => Boolean

  /** In parallel, counts the number of ways change can be made from the
   *  specified list of coins for the specified amount of money.
   */
  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int = {
    if (money <= 0 || threshold(money, coins) || coins.isEmpty) countChange(money, coins)
    else {
      val (numUseCoin: Int, numPassCoin: Int) = parallel(parCountChange(money, coins.tail, threshold), parCountChange(money - coins.head, coins, threshold))
      numUseCoin + numPassCoin
    }

  }

  /** Threshold heuristic based on the starting money. */
  def moneyThreshold(startingMoney: Int): Threshold = {
    //def inner(sMoney: Int)(rMoney: Int, coins: List[Int]):Boolean =
    //  rMoney <= sMoney.toDouble * 2.0 / 3.0
    //inner(startingMoney)
    (rMoney: Int, coins: List[Int]) => rMoney <= startingMoney * 2 / 3
  }

  /** Threshold heuristic based on the total number of initial coins. */
  def totalCoinsThreshold(totalCoins: Int): Threshold =
  (rMoney: Int, coins: List[Int]) => coins.length <= totalCoins.toDouble * 2.0 / 3.0


  /** Threshold heuristic based on the starting money and the initial list of coins. */
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold = {
    (rMoney: Int, coins: List[Int]) => moneyThreshold(startingMoney)(rMoney, coins) || totalCoinsThreshold(allCoins.length)(rMoney, coins)
  }
}
