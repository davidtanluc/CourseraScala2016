package reductions

import org.scalameter._
import common._

object ParallelCountChangeRunner {

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 80,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val amount = 250
    val coins = List(1, 2, 5, 10, 20, 50)
    val seqtime = standardConfig measure {
      seqResult = ParallelCountChange.countChange(amount, coins)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime ms")

    def measureParallelCountChange(threshold: ParallelCountChange.Threshold): Unit = {
      val fjtime = standardConfig measure {
        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
      }
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime ms")
      println(s"speedup: ${seqtime / fjtime}")
    }

    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))
  }
}

object ParallelCountChange {

  /** Returns the number of ways change can be made from the specified list of
   *  coins for the specified amount of money.
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    (money, coins) match {
      case (0, _) => 1
      case (m, _) if m < 0 => 0
      case (_, cs)  if cs.isEmpty => 0
      case (m, cs) => countChange(m - cs.head, cs) + countChange(m, cs.tail)
    }
  }

  type Threshold = (Int, List[Int]) => Boolean

  /** In parallel, counts the number of ways change can be made from the
   *  specified list of coins for the specified amount of money.
   */
  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int = {
    if(threshold(money, coins)) countChange(money, coins) //sequential
    else {
      val (leftValue, rightValue) = parallel(parCountChange(money - coins.head, coins, threshold),
                                             parCountChange(money, coins.tail, threshold))
      leftValue + rightValue
    }
  }

  /** Threshold heuristic based on the starting money. */
  /*
  For this reason, many parallel algorithms in practice rely on heuristics to assess the amount of work in a subtask.
  We will implement several such heuristics in this exercise, and assess the effect on performance.

  First, implement the moneyThreshold method, which creates a threshold function that returns true when the amount of money
  is less than or equal to 2 / 3 of the starting amount:
   */
  def moneyThreshold(startingMoney: Int): Threshold = (money: Int, coins: List[Int]) =>
                                                                    money <= (2 * startingMoney / 3) || coins.isEmpty
/*
The previous heuristic did not take into account how many coins were left on the coins list, so try two other heuristics.

Implement the method totalCoinsThreshold, which returns a threshold function that returns true when the number of
coins is less than or equal to the 2 / 3 of the initial number of coins:
 */

  /** Threshold heuristic based on the total number of initial coins. */
  def totalCoinsThreshold(totalCoins: Int): Threshold = (money: Int, coins: List[Int]) =>
                                                                  coins.length <= (2 * totalCoins / 3) || money <= 0

  /** Threshold heuristic based on the starting money and the initial list of coins. */
  /*
  * Then, implement the method combinedThreshold, which returns a threshold function that returns true when the amount
  * of money multiplied with the number of remaining coins is less than or equal to the starting money multiplied with the initial
  * number of coins divided by 2:
  * */
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold = (money: Int, coins: List[Int]) =>
                         moneyThreshold(startingMoney)(money,coins) || totalCoinsThreshold(allCoins.length)(money,coins)

  /*
  Which of the three threshold heuristics gives the best speedup?
  Can you think of a heuristic that improves performance even more?
   */
}
/*

measurements: 129.35399, 113.163969, 118.111098, 132.19068, 125.699015, 119.420301, 119.274824, 114.325925, 126.371774,
126.930128, 117.404091, 121.13075, 128.380248, 114.329921, 120.066309, 129.668288, 120.021587, 112.545638, 121.154522,
119.216216, 114.662619, 124.730057, 124.65232, 120.982044, 124.12758, 117.431287, 126.369656, 120.640802, 137.251341,
122.653336, 126.926018, 121.960683, 118.29134, 120.818172, 113.461888, 117.135196, 112.846301, 119.438842, 124.225534,
125.82417, 131.191113, 119.810649, 131.223221, 134.176889, 126.528431, 118.198722, 120.446031, 117.159207, 118.211716,
123.771327, 117.340868, 114.697524, 115.188138, 121.886064, 154.316883, 154.083663, 165.158256, 147.147333, 159.864366,
116.904568, 124.832839, 115.381101, 118.995068, 129.579463, 113.43394, 121.058769, 112.153261, 111.496016, 112.450029,
123.16852, 118.398783, 119.433795, 122.594136, 128.264347, 121.374812, 123.429031, 117.321158, 127.802101, 119.361173,
124.728642

sequential result = 177863
sequential count time: 123.44688016249998 ms

measurements: 33.912987, 38.397442, 32.097467, 41.342041, 40.297972, 33.051092, 35.632804, 33.788858, 36.659811, 33.660265,
 36.976704, 35.037701, 32.987843, 34.139968, 37.57147, 32.151236, 34.107608, 30.400699, 35.19543, 33.973225, 32.324073,
 36.452604, 38.04864, 31.78669, 33.997582, 38.21052, 33.793672, 33.325081, 38.883396, 40.53665, 33.656783, 36.159104,
 35.624457, 37.085939, 38.155262, 42.787887, 41.669794, 42.767704, 37.286499, 38.608704, 32.926848, 31.022449, 31.584995,
  31.23605, 38.06221, 37.823925, 36.192674, 32.334709, 34.291932, 38.67015, 37.530706, 35.109823, 32.075478, 30.490783,
  32.495611, 30.926747, 34.987502, 32.395454, 38.329063, 36.709219, 42.457473, 34.299784, 33.75145, 40.873906, 39.652243,
  33.478495, 39.336365, 37.203543, 37.561416, 34.241006, 30.903967, 32.904703, 33.324833, 31.313334, 35.105239, 36.891096,
  37.789023, 42.911261, 39.12147, 35.363876
parallel result = 177863
parallel count time: 35.702805937499996 ms
speedup: 3.457624041611785

measurements: 30.003846, 29.140345, 30.169473, 28.830148, 29.868368, 30.844198, 29.429136, 30.87035, 28.03163, 28.095018,
28.319623, 27.667463, 28.720006, 27.760608, 28.774892, 27.340681, 28.74951, 27.399648, 28.87389, 27.381397, 29.131048,
28.413684, 29.398278, 27.82838, 30.505441, 28.108864, 28.629208, 28.081219, 27.794281, 29.254069, 27.541608, 29.302005,
27.683737, 28.881418, 28.142009, 27.661759, 29.501492, 27.554993, 29.293094, 28.629105, 31.884152, 28.399738, 28.436295,
30.145159, 29.076512, 29.404088, 28.633211, 29.01839, 29.220552, 28.275555, 29.739202, 28.217112, 28.630903, 29.022217,
28.983885, 29.164177, 28.200338, 28.332475, 36.052679, 28.984389, 29.822255, 28.185335, 28.72101, 28.522434, 28.311589,
29.247278, 28.703068, 30.21892, 28.067647, 30.212976, 28.377569, 27.551764, 29.235942, 28.612665, 29.089632, 28.249687,
29.427725, 28.480895, 27.911954, 29.743499
parallel result = 177863
parallel count time: 28.90150993750001 ms
speedup: 4.271295182482019


measurements: 36.974495, 36.277826, 35.609054, 35.514329, 32.263176, 32.129925, 32.790309, 31.187837, 35.524217, 35.724063,
30.067413, 35.947476, 31.364281, 35.706554, 33.027886, 34.85041, 41.293717, 39.941974, 38.295126, 32.690517, 35.973489,
 36.664887, 37.84296, 44.410732, 47.474107, 39.35205, 45.970487, 37.975523, 44.627393, 37.031734, 41.797022, 41.775585,
 39.464705, 38.305842, 39.194818, 41.520165, 33.34716, 31.24583, 32.480236, 33.978079, 28.794069, 35.993433, 36.075222,
 32.244533, 33.176985, 33.055802, 35.430894, 40.346662, 38.714574, 39.734979, 36.856189, 31.275564, 31.158834, 32.352654,
 30.621038, 31.211748, 32.702083, 30.092468, 31.257099, 29.342332, 30.081093, 40.851776, 36.675312, 34.819788, 30.264938,
 36.712887, 36.137404, 32.545854, 31.375553, 33.935665, 30.161982, 35.607549, 34.40853, 34.316197, 31.389789, 32.853219,
 41.129288, 35.993424, 41.764862, 33.769128
parallel result = 177863
parallel count time: 35.535559862499994 ms
speedup: 3.47389715091477
 */