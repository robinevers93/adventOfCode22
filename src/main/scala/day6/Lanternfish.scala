package day6

object Lanternfish {

  def getStartingState(list: List[Int]): Map[Int, Long] =
    list.groupBy(identity).map(x => x._1 -> x._2.size.toLong)

  def getNewState(m: Map[Int, Long]): Map[Int, Long] = {
    val m1: Map[Int, Long] = m.removed(0).map(x => (x._1 -1, x._2))
    val m2: Map[Int, Long] = Map(6 -> (m1.getOrElse(6, 0L) + m.getOrElse(0, 0L)), 8  -> m.getOrElse(0, 0L))
    m1 ++ m2
  }

  def afterNDays(state: List[Int], n: Int): Long =
    Function.chain(List.fill(n)(getNewState _))(getStartingState(state))
      .values.toList.sum

}
