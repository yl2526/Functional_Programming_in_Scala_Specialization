def scanLeft[A](inp: Array[A],
                a0: A, f: (A,A) => A,
                out: Array[A]): Unit = {
  out(0) = a0
  var a = a0
  var i = 0
  while (i < inp.length) {
    a = f(a, inp(i))
    i = i + 1
    out(i) = a
  }
}
val in= Array(2,3,4,5,6)
var outSeq= Array(0,0,0,0,0,0)
val f= (x:Int, y:Int) => x + y

scanLeft(in, 100, f, outSeq)

sealed abstract class Tree[A]
case class Leaf[A](a: A) extends Tree[A]
case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]


sealed abstract class TreeRes[A] { val res: A }
case class LeafRes[A](override val res: A) extends TreeRes[A]
case class NodeRes[A](l: TreeRes[A],
                      override val res: A,
                      r: TreeRes[A]) extends TreeRes[A]

def reduceRes[A](t: Tree[A], f: (A,A) => A): TreeRes[A] = t match {
  case Leaf(v) => LeafRes(v)
  case Node(l, r) => {
    val (tL, tR) = (reduceRes(l, f), reduceRes(r, f))
    NodeRes(tL, f(tL.res, tR.res), tR)
  }
}

val t1 = Node(Node(Leaf(1), Leaf(3)), Node(Leaf(8), Leaf(50)))
val plus = (x:Int,y:Int) => x+y
reduceRes(t1, plus)


def upsweep[A](t: Tree[A], f: (A,A) => A): TreeRes[A] = t match {
  case Leaf(v) => LeafRes(v)
  case Node(l, r) => {
    val (tL, tR) = parallel(upsweep(l, f), upsweep(r, f))
    NodeRes(tL, f(tL.res, tR.res), tR)
  }
}

val t1 = Node(Node(Leaf(1), Leaf(3)), Node(Leaf(8), Leaf(50)))
val plus = (x:Int,y:Int) => x+y
val res0 = reduceRes(t1, plus)

def downsweep[A](t: TreeRes[A], a0: A, f : (A,A) => A): Tree[A] = t match {
  case LeafRes(a) => Leaf(f(a0, a))
  case NodeRes(l, _, r) => {
    val (tL, tR) = parallel(downsweep[A](l, a0, f),
      downsweep[A](r, f(a0, l.res), f))
    Node(tL, tR) } }

downsweep(res0, 100, plus)

def scanLeft[A](t: Tree[A], a0: A, f: (A,A) => A): Tree[A] = {
  val tRes = upsweep(t, f)
  val scan1 = downsweep(tRes, a0, f)
  prepend(a0, scan1)
}

def prepend[A](x: A, t: Tree[A]): Tree[A] = t match {
  case Leaf(v) => Node(Leaf(x), Leaf(v))
  case Node(l, r) => Node(prepend(x, l), r)
}


// Intermediate tree for array reduce
sealed abstract class TreeResA[A] { val res: A }
case class Leaf[A](from: Int, to: Int, // only track the index
                   override val res: A) extends TreeResA[A]
case class Node[A](l: TreeResA[A],
                   override val res: A,
                   r: TreeResA[A]) extends TreeResA[A]

def upsweep[A](inp: Array[A], from: Int, to: Int, f: (A,A) => A,
               threshold: Int): TreeResA[A] = {
  if (to - from < threshold)
    Leaf(from, to, reduceSeg1(inp, from + 1, to, inp(from), f))
  else {
    val mid = from + (to - from)/2
    val (tL,tR) = parallel(upsweep(inp, from, mid, f),
      upsweep(inp, mid, to, f))
    Node(tL, f(tL.res,tR.res), tR)
  }
}

def reduceSeg1[A](inp: Array[A], left: Int, right: Int,
                  a0: A, f: (A,A) => A): A = {
  var a= a0
  var i= left
  while (i < right) {
    a= f(a, inp(i))
    i= i+1
  }
  a
}

def downsweep[A](inp: Array[A],
                 a0: A, f: (A,A) => A,
                 t: TreeResA[A],
                 out: Array[A]): Unit = t match {
  case Leaf(from, to, res) =>
    scanLeftSeg(inp, from, to, a0, f, out)
  case Node(l, _, r) => {
    val (_,_) = parallel(
      downsweep(inp, a0, f, l, out),
      downsweep(inp, f(a0,l.res), f, r, out))
  }
}

def scanLeftSeg[A](inp: Array[A], left: Int, right: Int,
                   a0: A, f: (A,A) => A,
                   out: Array[A]) = {
  if (left < right) {
    var i= left
    var a= a0
    while (i < right) {
      a= f(a,inp(i))
      i= i+1
      out(i)=a
    }
  }
}

def scanLeft[A](inp: Array[A],
                a0: A, f: (A,A) => A,
                out: Array[A]) = {
  val t = upsweep(inp, 0, inp.length, f, 3)
  downsweep(inp, a0, f, t, out) // fills out[1..inp.length]
  out(0)= a0 // prepends a0
}