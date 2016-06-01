package tan

/**
  * Created by davidtan on 5/31/16.
  */

import common._

object mergesort extends App{
  def parMergeSort(xs: Array[Int], maxDepth: Int): Unit = {
    def  quickSort(xs: Array[Int], offset: Int, length: Int): Unit = ???
    val ys = Array[Int](xs.length)
    def merge(src: Array[Int], dst: Array[Int], from: Int, mid: Int, until: Int): Unit = {
      var left = from
      var right = mid
      var i = from
      while (left < mid && right < until) {
        while (left < mid && src(left) <= src(right)) {
          dst(i) = src(left)
          i += 1
          left += 1
        }
        while (right < until && src(right) <= src(left)) {
          dst(i) = src(right)
          i += 1
          right += 1
        }
      }
      while (left < mid) {
        dst(i) = src(left)
        i += 1
        left += 1
      }
      while (right < mid) {
        dst(i) = src(right)
        i += 1
        right += 1
      }
    }

    def sort(from: Int, until: Int, depth: Int): Unit = {
      if (depth == maxDepth) {
        quickSort(xs, from, until - from)
      } else {
        val mid = (from + until) / 2
        parallel(sort(mid, until, depth + 1), sort(from, mid, depth + 1))
        val flip = (maxDepth - depth) % 2 == 0
        val src = if (flip) ys else xs
        val dst = if (flip) xs else ys
        merge(src, dst, from, mid, until)
      }
    }
    sort(0, xs.length, 0)

    def copy(src: Array[Int], target: Array[Int], from: Int, until: Int, depth: Int): Unit = {
      if (depth == maxDepth) {
        Array.copy(src, from, target, from, until - from)
      } else {
        val mid = (from + until) / 2
        parallel(
          copy(src, target, mid, until, depth + 1),
          copy(src, target, from, mid, depth + 1)
        )
      }
    }
    copy(ys, xs, 0, xs.length, 0)
    println(ys)
  }

  parMergeSort(Array(1,4,4,8,6,4,3,4,5,6), 5)
}
