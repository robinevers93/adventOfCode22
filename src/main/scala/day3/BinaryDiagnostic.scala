package day3

import scala.annotation.tailrec

object BinaryDiagnostic {

  def getGammaRate(list: List[List[Int]]): List[Int] =
    list.transpose.map(x => if (x.sum < list.length/2) 0 else 1)

  def getEpsilonRate(gammaRate: List[Int]): List[Int] =
    gammaRate.map(x => if (x == 0) 1 else 0)

  def multiplyBinaryNumbers(gammaRate: List[Int], epsilonRate: List[Int]): Int =
    Integer.parseInt(gammaRate.mkString, 2) * Integer.parseInt(epsilonRate.mkString, 2)

  val oxygenCriteria: List[Int] => Int => Boolean =
    list => if (list.sum < list.length/2.0) _ == 0 else _ == 1

  val co2Criteria: List[Int] => Int => Boolean =
    list => i => !oxygenCriteria(list)(i)

  @tailrec
  def filterList(list: List[List[Int]], i: Int, bitCriteria: List[Int] => Int => Boolean): List[Int] =
    if (list.length == 1)
      list.head
    else
      filterList(list.filter(x => bitCriteria(list.map(_(i)))(x(i))), i + 1, bitCriteria)

}
