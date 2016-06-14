package calculator

object Polynomial {
  //Δ = b² - 4ac
  //http://www.themathpage.com/aprecalc/complete-the-square.htm
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {
    Signal({
      val b_sig = b()
      (b_sig * b_sig) - 4*(a()*c())
    })
  }
//(-b ± √Δ) / 2a-->
  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal({
      def findCompleteQuadratic(Δ:Double): Double = (-b()+ Δ)/ (2*a())

      delta() match {
        case x if x>0 => val sqrt = Math.sqrt(x);Set(findCompleteQuadratic(-sqrt),findCompleteQuadratic(sqrt))
        case y if y<0 => Set(findCompleteQuadratic(0))
        case _ => Set()
      }
    })
  }
}
