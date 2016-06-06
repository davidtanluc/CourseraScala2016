package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    val result =chars.foldLeft(0,true)(
      {
        case(acc,r)=>(acc,r)match{
          case((count,status),'(') =>(count +1,status)
          case((count,s),')') =>(count -1,if((count-1)<0) false else true)
          case(_,_)=>acc
        }
      }

    )
    if(result._1 ==0 && result._2)return true
    false
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    type Element = (Int, Int)

    @tailrec
    def loop(idx: Int, until: Int, arg1: Int, arg2: Int) : Element = {
      if (idx >= until) (arg1, arg2)
      else chars(idx) match {
        case ')' => loop(idx + 1, until, arg1 - 1, arg2 min (arg1 - 1))
        case '(' => loop(idx + 1, until, arg1 + 1, arg2)
        case _ => loop(idx + 1, until, arg1, arg2)
      }
    }

    def reduce(from: Int, until: Int): Element = {
      if(until - from <= threshold) loop(from, until, 0, 0)
      else {
        val middle = (from + until) / 2
        val (left, right) = parallel(reduce(from, middle),
                                     reduce(middle, until))
        reduceElements(left, right)
      }
    }

    def reduceElements(a: Element, b: Element): Element =
                                   (a._1 + b._1, a._2 min (a._2 - b._2))
        reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
/*
sequential result = true
sequential balancing time: 140.71073053333333 ms
Starting warmup.

parallel result = true
parallel balancing time: 59.37701935 ms
speedup: 2.369784338683436
 */