package scalashop

import org.scalameter._
import common._

object HorizontalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}


/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
   *  starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each row, `blur` traverses the pixels by going from left to right.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    for (curX <- 0 until src.width; curY <- from until end)
      dst.update(curX,curY,boxBlurKernel(src,curX,curY,radius))
  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  rows.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val max_tasks = (src.height / numTasks) max 1
    val separate_strips = 0 to src.height by max_tasks

    //println("separate_strips : ",separate_strips)

    separate_strips.zip(separate_strips.tail).map({
      case(start,stop)=> task[Unit]{blur(src,dst,start,stop,radius)}
    }).foreach(_.join())
  }

}

/*

[info] Running scalashop.HorizontalBoxBlurRunner
Starting warmup.
0. warmup run running time: 324.032158 (covNoGC: NaN, covGC: NaN)
1. warmup run running time: 301.575892 (covNoGC: 0.0508, covGC: 0.0508)
2. warmup run running time: 314.665514 (covNoGC: 0.0360, covGC: 0.0360)
3. warmup run running time: 312.093363 (covNoGC: 0.0295, covGC: 0.0295)
4. warmup run running time: 318.361607 (covNoGC: 0.0265, covGC: 0.0265)
Steady-state detected.
Ending warmup.
measurements: 314.681287, 317.053545, 305.769637, 311.972901, 312.970535, 312.757294, 307.856377, 320.432432, 299.503022, 308.067134
sequential blur time: 311.1064164 ms
Starting warmup.
0. warmup run running time: 93.000747 (covNoGC: NaN, covGC: NaN)
1. warmup run running time: 81.917295 (covNoGC: 0.0896, covGC: 0.0896)
2. warmup run running time: 83.494579 (covNoGC: 0.0696, covGC: 0.0696)
3. warmup run running time: 87.295787 (covNoGC: 0.0570, covGC: 0.0570)
4. warmup run running time: 83.389631 (covNoGC: 0.0522, covGC: 0.0522)
Steady-state detected.
Ending warmup.
measurements: 76.827535, 79.491404, 81.291928, 71.016448, 68.657647, 70.914964, 69.444319, 70.600577, 80.435599, 82.973606
fork/join blur time: 75.1654027 ms
speedup: 4.138957621788941
[success] Total time: 9 s, completed May 28, 2016 10:13:02 AM

 */
