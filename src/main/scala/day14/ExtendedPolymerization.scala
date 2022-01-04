package day14

import scala.util.matching.Regex

object ExtendedPolymerization {

  type Insertions = Map[List[Char], Char]

  private val regex: Regex = """([A-Z])([A-Z]) -> ([A-Z])""".r

  def parseRules(input: List[String]): Insertions =
    input.map {case regex(x, y, z) => Map(List(x.head, y.head) -> z.head)}.reduce(_ ++ _)

  def polymerise(startingSequence: List[Char], insertions: Insertions): List[Char] =
    startingSequence.sliding(2).toList.flatMap( x => List(insertions(x), x.last))
      .prepended(startingSequence.head)

  def polymeriseN(startingSequence: List[Char], insertions: Insertions, n: Int): List[Char] =
    Function.chain(List.fill(n)(polymerise(_, insertions)))(startingSequence)

  def getScore(sequence: List[Char]): Int = {
    val x = sequence.groupBy(identity).map(_._2.size)
    x.max - x.min
  }

}
