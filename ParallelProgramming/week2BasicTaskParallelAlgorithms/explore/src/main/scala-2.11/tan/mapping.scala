package tan

/**
  * Created by davidtan on 5/31/16.
  */
import common._

object mapping extends App{
  def mapASegSeq[A, B](inp: Array[A], left: Int, right: Int, f: A => B, out: Array[B]) = {
    var i = left
    while (i < right) {
      out(i) = f(inp(i))
      i = i + 1
    }
  }

  def mapASegPar[A, B](inp: Array[A], left: Int, right: Int, f: A => B, out: Array[B]): Unit = {
    if (right - left < 2)
      mapASegSeq(inp, left, right, f, out)
    else {
      val mid = left + (right - left) / 2
      parallel(mapASegPar(inp, left, mid, f, out),
        mapASegPar(inp, mid, right, f, out))
    }
  }

  val inp = Array(2,3,4,5,6)
  val out= Array[Double](0,0,0,0,0)
  val p: Double = 1.5
  def f(x: Int): Double = Math.pow(x, p)
  mapASegSeq(inp, 0, inp.length, f, out) // sequential
  mapASegPar(inp, 0, inp.length, f, out)
  def prn(x: TraversableOnce[_]) = println(x.mkString(x.getClass.getSimpleName + "(", ", ", ")"))

  prn(out)//ofDouble(2.8284271247461903, 5.196152422706632, 8.0, 11.180339887498949, 14.696938456699069)
}
