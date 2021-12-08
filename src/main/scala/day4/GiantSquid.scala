package day4

import scala.annotation.tailrec

object GiantSquid {

  def getCompletionRound(puzzle: List[List[Int]], numbers: List[Int]): Int = {
    @tailrec
    def go(remainingPuzzle: List[List[Int]], remainingNumbers: List[Int]): Int =
      if (remainingPuzzle.contains(Nil))
        remainingNumbers.length
      else
        go(remainingPuzzle.map(_.filter(_ != remainingNumbers.head)), remainingNumbers.tail)
    numbers.length - go(puzzle, numbers)
  }

  private def getCompletionRounds(puzzles: List[List[List[Int]]], numbers: List[Int]): List[Int] =
    puzzles.map(getCompletionRound(_, numbers))
      .zip(puzzles.map(puzzle => getCompletionRound(puzzle.transpose, numbers)))
      .map(x => if (x._1 < x._2) x._1 else x._2)

  def getWinner(puzzles: List[List[List[Int]]], numbers: List[Int]): (Int, Int) =
    getCompletionRounds(puzzles, numbers).zipWithIndex.min

  def getLoser(puzzles: List[List[List[Int]]], numbers: List[Int]): (Int, Int) =
    getCompletionRounds(puzzles, numbers).zipWithIndex.max

  def getScore(puzzle: List[List[Int]], calledNumbers: List[Int]): Int =
    puzzle.flatten.filterNot(calledNumbers.contains(_)).sum * calledNumbers.last

}
