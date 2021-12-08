package day7

object Whales {

  def fuelCost(positions: List[Int], align: Int): Int =
    positions.map(p => Math.abs(p - align)).sum

  def fuelCost2(positions: List[Int], align: Int): Int =
    positions.map(p => (Math.abs(p - align) * (Math.abs(p - align) + 1)) / 2).sum

  def fuelRequired(positions: List[Int], fuelCost: (List[Int], Int) => Int): Int =
    Range.inclusive(0, positions.max).map(a=> fuelCost(positions, a)).min


  //wanted to do maths, but scala doesn't have .roots, which probably makes this solution worse
  val f: (Int, List[Int]) => Double = (x, as) => as.map(a => if (x != a) (x - a) / Math.abs(a - x).toDouble else 0).sum
  val f2: (Int, List[Int]) => Double = (x, as) => as.map(a => if (x != a) (a - x) * (2 * Math.abs(a - x) + 1)/(2 * Math.abs(a - x).toDouble) else 0).sum

  def optimalPosition(as: List[Int], derivativeOfCostFunction: (Int, List[Int]) => Double): Int =
    Range.inclusive(0, as.max).map(derivativeOfCostFunction(_, as)).map(_.abs).zipWithIndex.minBy(_._1)._2

}
