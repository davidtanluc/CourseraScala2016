package tan

object reduce {
  import common._

  sealed abstract class Tree[A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  //SEQUENTIAL
  def reduce[A](t: Tree[A], f: (A, A) => A): A = t match {
    case Leaf(v) => v
    case Node(l, r) => f(reduce[A](l, f), reduce[A](r, f)) // Node -> f
  }

  val tree = Node(Leaf(1), Node(Leaf(3), Leaf(8)))
  def diff = (x: Int, y: Int) => x - y
  val res = reduce(tree, diff)

  //PARALLEL
  def reducePar[A](t: Tree[A], f: (A, A) => A): A = t match {
    case Leaf(v) => v
    case Node(l, r) =>
      val (left, right) = parallel(reducePar(l, f), reducePar(r, f))
      f(left, right)
  }

  ////////////////////
  def map[A, B](t: Tree[A], f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Node(l, r) => Node(map[A, B](l, f), map[A, B](r, f))
  }

  def toList[A](t: Tree[A]): List[A] = t match {
    case Leaf(v) => List(v)
    case Node(l, r) => toList[A](l) ++ toList[A](r)
  }

  def toList2[A](t: Tree[A]): List[A] = {
    def f(a: A): List[A] = List(a)
    def f2(a: List[A], b: List[A]): List[A] = a ++ b
    reduce(map(t, f), f2)
  }

  toList(tree) == toList2(tree)
  ///////////////////////////////

  //SEQUENTIAL
  def reduceArray[A](inp: Array[A], f: (A, A) => A): A = {
    def reduceSegArray(inp: Array[A], left: Int, right: Int, f: (A, A) => A): A = {
      if (right - left < 5) {
        var res = inp(left)
        var i = left + 1
        while (i < right) {
          res = f(res, inp(i))
          i = i + 1
        }
        res
      } else {
        val mid = left + (right - left) / 2
        val (a1, a2) = parallel(
          reduceSegArray(inp, left, mid, f),
          reduceSegArray(inp, mid, right, f)
        )
        f(a1, a2)
      }
    }
    reduceSegArray(inp, 0, inp.length, f)
  }

}
