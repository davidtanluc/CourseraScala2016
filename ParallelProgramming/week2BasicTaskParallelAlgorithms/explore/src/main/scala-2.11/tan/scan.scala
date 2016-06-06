package tan

/**
  * Created by davidtan on 6/4/16.
  */
object scan {
  import common._

  def reduceSeg1[A](inp: Array[A], left: Int, right: Int,
                    a0: Int, f: (A, A) => A): A = ???

  //Trees storing our input collection only have values in leaves:
  //Intermediate tree for array reduce
  sealed abstract class TreeResA[A] {
    val res: A
  }
  case class Leaf[A](from: Int, to: Int,
                     override val res: A) extends TreeResA[A]
  case class Node[A](l: TreeResA[A],
                     override val res: A,
                     r: TreeResA[A]) extends TreeResA[A]
  //Starts from an array, produces a tree
  def upsweep[A](inp: Array[A], from: Int, to: Int,
                 f: (A, A) => A): TreeResA[A] = {
    if (to - from < threshold)
      Leaf(from, to, reduceSeg1(inp, from + 1, to, inp(from), f))
    else {
      val mid = from + (to - from) / 2
      val (tL, tR) = parallel(upsweep(inp, from, mid, f),
        upsweep(inp, mid, to, f))
      Node(tL, f(tL.res, tR.res), tR)
    }
  }
  //Sequential reduce for segment
  def reduceSeg1[A](inp: Array[A], left: Int, right: Int,
                    a0: A, f: (A, A) => A): A = {
    var a = a0
    var i = left
    while (i < right) {
      a = f(a, inp(i))
      i = i + 1
    }
    a
  }
  //Downsweep on array
  def downsweep[A](inp: Array[A], a0: A, f: (A, A) => A, t: TreeResA[A], out: Array[A]): Unit = t match {
    case Leaf(from, to, res) =>
      scanLeftSeg(inp, from, to, a0, f, out)
    case Node(l, _, r) =>
      val (_, _) = parallel(
        downsweep(inp, a0, f, l, out),
        downsweep(inp, f(a0, l.res), f, r, out))
  }
  //Sequential scan left on segment
  //Writes to output shifted by one.
  def scanLeftSeg[A](inp: Array[A], left: Int, right: Int, a0: A, f: (A, A) => A, out: Array[A]) = {
    if (left < right) {
      var i = left
      var a = a0
      while (i < right) {
        a = f(a, inp(i))
        i = i + 1
        out(i) = a
      }
    }
  }
  //Finally: parallel scan on the array
  def scanLeft[A](inp: Array[A], a0: A, f: (A, A) => A, out: Array[A]) = {
    val t = upsweep(inp, 0, inp.length, f)
    downsweep(inp, a0, f, t, out) // fills out[1..inp.length]
    out(0) = a0 // prepends a0
  }
}
