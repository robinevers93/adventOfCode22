package day14

case class ExtendedPolymerization2(startingSequence: String, input: List[String]) {

  type Insertions = Map[List[Char], Char]
  type PairCount = Map[List[Char], Long]

  private val initialPairs: PairCount = startingSequence.toList.sliding(2).toList.groupMapReduce(identity)(_ => 1L)(_ + _)
  private val rules: Insertions = ExtendedPolymerization.parseRules(input)

  def addPairsToCount(pairs: PairCount, pairsToAdd: List[(List[Char], Long)]): PairCount = pairsToAdd match {
    case (key, value) :: t =>
      val pair1 = List(key.head, rules(key))
      val pair2 = List(rules(key), key.last)
      val newPairs = pairs
        .updated(pair1, value + pairs.getOrElse(pair1, 0L))
        .updated(pair2, value + pairs.getOrElse(pair2, 0L))
      addPairsToCount(newPairs, t)
    case Nil => pairs
  }

  def polymerise(pairs: PairCount): PairCount =
    addPairsToCount(Map(), pairs.toList)

  def polymeriseN(initialPairs: PairCount, n: Int): PairCount =
    Function.chain(List.fill(n)(polymerise _))(initialPairs)

  def pairsToAnswer(pairs: PairCount, firstChar: Char, lastChar: Char): Long = {
    val chars = pairs.flatMap{
      case List(a, b) -> x => List(Map(a -> x), Map(b -> x))
      case _ => List()
    }
      .toList
      .prepended(Map(firstChar -> 1L))
      .prepended(Map(lastChar -> 1L))
      .flatten
      .groupMapReduce(_._1)(_._2)(_ + _)

    (chars.values.max - chars.values.min)/2
  }

  def getAnswer: Long = pairsToAnswer(polymeriseN(initialPairs, 40), startingSequence.head, startingSequence.last)

}
