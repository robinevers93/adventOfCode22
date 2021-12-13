package day10

object SyntaxScoring {

  def isLegalPair(x: Char, y: Char): Boolean = (x,y) match {
    case ('<', '>') | ('(', ')') | ('[', ']') | ('{', '}') => true
    case _ => false
  }

  def removeLegalPairsOnce(line: List[Char]): List[Char] = {
    val indicesToFilter = line.zipWithIndex.sliding(2).collect {
      case Seq(a,b) if isLegalPair(a._1,b._1) => List(a._2,b._2)
      case _ => List()
    }.toList.flatten
    line.zipWithIndex.filterNot(i => indicesToFilter.contains(i._2)).map(_._1)
  }

  def removeLegalPairs(line: List[Char]): List[Char] =
    Function.chain(List.fill(line.length / 2)(removeLegalPairsOnce _))(line)

  def getSyntaxErrorScore(line: List[Char]): Int =
    removeLegalPairs(line).find(Set('>', ')', '}', ']')) match {
      case Some(')') => 3
      case Some(']') => 57
      case Some('}') => 1197
      case Some('>') => 25137
      case _ => 0
    }

  def getIncompleteLines(input: List[List[Char]]): List[List[Char]] =
    input.filter(lines => getSyntaxErrorScore(lines) == 0 && removeLegalPairs(lines) != List())

  def getCompletionValue(c: Char): Long = c match {
    case '[' => 2L
    case '(' => 1L
    case '{' => 3L
    case '<' => 4L
  }

  def getAutocompleteScore(line: List[Char]): Long =
    removeLegalPairs(line).reverse.map(getCompletionValue)
      .prepended(0L)
      .reduceLeft((a,b) => 5*(a+b))/5

  def getMiddleScore(list: List[Long]): Long =
    list.sorted.apply((list.length-1)/2)

}
