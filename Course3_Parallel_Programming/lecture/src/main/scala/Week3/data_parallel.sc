(1 until 1000)
  .par
  .filter(n => n % 3 == 0)
  .count(n => n.toString == n.toString.reverse)

def sum(xs: Array[Int]): Int = {
  xs.par.fold(0)(_ + _)
}

sum(Array(1,2,3,4))

def max(xs: Array[Int]): Int = {
  xs.par.fold(Int.MinValue)(math.max)
}

max(Array(1,2,3,4))


def play(a: String, b: String): String = List(a, b).sorted match {
  case List("paper", "scissors") => "scissors"
  case List("paper", "rock") => "paper"
  case List("rock", "scissors") => "rock"
  case List(a, b) if a == b => a
  case List("", b) => b
}

// not associative
Array("paper", "rock", "paper", "scissors")
.par.fold("")(play)


def isVowel(a: Char): Boolean =
  Set('A', 'E', 'I', 'O', 'U').contains(a)

Array('E', 'P', 'F', 'L', 'A').par
  .foldLeft(0)((count, c) => if (isVowel(c)) count + 1 else count)
// but fold doesn't compile because fold need output, input of same type

Array('E', 'P', 'F', 'L', 'A').par
  .aggregate(0)(
  (count, c) => if (isVowel(c)) count + 1 else count, //sequential fold function
  _ + _ //parallel fold function
)

