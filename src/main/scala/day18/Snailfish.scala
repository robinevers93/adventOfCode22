package day18

object Snailfish {

  def parseSnailfishNumber(snailfishNumber: (Any, Any)): SnailfishNumber = snailfishNumber match {
    case (x: Int,y: Int) => PairedNumber(SimpleNumber(x), SimpleNumber(y))
    case (x: Int, y: (Any, Any)) => PairedNumber(SimpleNumber(x), parseSnailfishNumber(y))
    case (x: (Any, Any), y: Int) => PairedNumber(parseSnailfishNumber(x), SimpleNumber(y))
    case (x: (Any, Any), y: (Any, Any)) => PairedNumber(parseSnailfishNumber(x), parseSnailfishNumber(y))
  }

  def getLargestMagnitude(sfns: List[SnailfishNumber]): Int = {
    val pairs: List[List[SnailfishNumber]] = sfns.combinations(2).toList
    val test: List[SnailfishNumber] = pairs.map(_.reduce(_ add _))
    test.map(_.magnitude).max
  }

}
