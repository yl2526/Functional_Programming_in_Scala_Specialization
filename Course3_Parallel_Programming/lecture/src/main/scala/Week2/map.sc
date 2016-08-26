
def mapASegSeq[A,B](inp: Array[A], left: Int, right: Int, f : A => B,
                    out: Array[B]) = {
  // Writes to out(i) for left <= i <= right-1
  var i= left
  while (i < right) {
    out(i)= f(inp(i))
    i= i+1
  } }
val in= Array(2,3,4,5,6)
val outSeq= Array(0,0,0,0,0)
val f= (x:Int) => x*x
mapASegSeq(in, 1, 3, f, outSeq)

outSeq

import common._

def mapASegPar[A,B](inp: Array[A], left: Int, right: Int, f : A => B,
                    out: Array[B]): Unit = {
  // Writes to out(i) for left <= i <= right-1
  if (right - left < threshold)
    mapASegSeq(inp, left, right, f, out)
  else {
    val mid = left + (right - left)/2
    parallel(mapASegPar(inp, left, mid, f, out),
      mapASegPar(inp, mid, right, f, out))
  }
}
val in= Array(2,3,4,5,6)
var outPar= Array(0,0,0,0,0)
val f= (x:Int) => x*x
mapASegSeq(in, 1, 3, f, outPar)

outPar



sealed abstract class Tree[A] { val size: Int }
case class Leaf[A](a: Array[A]) extends Tree[A] {
  override val size = a.size
}
case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A] {
  override val size = l.size + r.size
}


def mapTreePar[A:Manifest,B:Manifest](t: Tree[A], f: A => B) : Tree[B] =
  t match {
    case Leaf(a) => {
      val len = a.length; val b = new Array[B](len)
      var i= 0
      while (i < len) { b(i)= f(a(i)); i= i + 1 }
      Leaf(b) }
    case Node(l,r) => {
      val (lb,rb) = parallel(mapTreePar(l,f), mapTreePar(r,f))
      Node(lb, rb) }
  }