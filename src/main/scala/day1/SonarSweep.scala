package day1

object SonarSweep {

  def numberOfIncreases(list: List[Int]): Int =
    list.zip(list.tail).count(y => y._1 < y._2)

  def measurementsAsThreeMeasurementWindow(list: List[Int]): List[Int] =
    list.lazyZip(list.tail).lazyZip(list.tail.tail).toList.map(x => x._1 + x._2 + x._3)

}
