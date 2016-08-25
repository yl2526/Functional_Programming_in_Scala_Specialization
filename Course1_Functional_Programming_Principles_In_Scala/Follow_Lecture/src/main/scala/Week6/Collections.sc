val nums = Vector(1, 2, 3, -88)
val people = Vector("Bob", "James", "Peter")

9 +: nums
nums :+ 9


val xs: Array[Int] = Array(-1, 1, 2, 3)
xs map (x => 2 * x)

val ys: String = "Hello world!"
ys filter (_.isUpper)

val r: Range = 1 until 5
val rt: Range = 1 to 5
1 to 10 by 3
6 to 1 by -2

xs exists (_ > 0)
xs forall (_ > 0)
xs zip ys
(xs zip nums).unzip
r.flatMap(c => List('.', c))
xs.sum
xs.product
xs.max
xs.min


(1 to 10) flatMap ( x=> (1 to 3) map (y => (x, y)))

(xs zip nums).map(xy => xy._1 * xy._2).sum
(xs zip nums).map{case (x, y) => x * y}.sum

def isPrime(n: Int): Boolean = (2 until n) forall (d => n%d != 0)

// combinatorial
((1 until 5) flatMap (i =>
  (1 until i) map (j => (i, j)))) filter(pair => isPrime(pair._1 + pair._2))

for {
  i <- 1 until 5
  j <- 1 until i
  if isPrime(i + j)
} yield (i, j)


// Sets
val fruit = Set("apple", "banana", "pear", "pineapple")
val s = (1 to 6).toSet

s map (_ + 2)
fruit filter (_.startsWith("app"))
s.nonEmpty

def queens(n: Int) = {
  def placeQueens(k: Int): Set[List[Int]] = {
    if (k == 0) Set(List())
    else
      for {
        queens <- placeQueens(k - 1)
        col <- 0 until n
        if isSafe(col, queens)
      } yield col :: queens
  }
  placeQueens(n)
}

def isSafe(col: Int, queens: List[Int]): Boolean ={
  val row = queens.length
  val queensWithRow = (row -1 to 0 by -1) zip queens
  queensWithRow forall {
    case (r, c) => col != c && math.abs(col - c) != row - r
  }
}

def show(queens: List[Int]) = {
  val lines =
    for (col <- queens.reverse)
      yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
  "\n" + (lines mkString "\n")
}

queens(4) take 3 map show mkString "\n"


























