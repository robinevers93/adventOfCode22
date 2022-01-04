package day21

import zio.Runtime
import zio.stream.Stream

object DiracDice {

  val dice: List[Int] = List.fill(100)(Range.inclusive(1, 100).toList).flatten

  case class GameState1(pos1: Int, pos2: Int, dice: List[Int], turn: Int = 0, points1: Int = 0, points2: Int = 0){

    def incrementPosition: GameState1 = {
      val newPos1 = (pos1 + dice.take(3).sum - 1) % 10 + 1
      val newPos2 = (pos2 + dice.slice(3, 6).sum - 1) % 10 + 1

      GameState1(newPos1, newPos2, dice.drop(6), turn + 1, points1 + newPos1, points2 + newPos2)
    }

    def incrementPositionB(move1: Int, move2: Int): GameState1 = {
      val newPos1 = (pos1 + dice.head - 1) % 10 + 1
      val newPos2 = (pos2 + dice(1) - 1) % 10 + 1

      GameState1(newPos1, newPos2, List(move1, move2), turn + 1, points1 + newPos1, points2 + newPos2)
    }

    def getFinalGameState(gameEnd: Int): Option[GameState1] =
      Runtime.default.unsafeRun(
        Stream.iterate(this)(_.incrementPosition)
          .takeUntil(status => status.points1 >= gameEnd || status.points2 >= gameEnd)
          .runLast
      )

    def getScore: Int =
      if (points1 < points2)
        (points1 - pos1) * turn * 6
      else
        (points2 - pos2) * (turn * 6 - 3)
  }

  def part1(pos1: Int, pos2: Int): Int =
    GameState1(pos1, pos2, dice).getFinalGameState(1000).get.getScore


  // PART 2

  case class GameState2(pos1: Int, pos2: Int, points1: Int, points2: Int, player1Turn: Boolean, rollsLeftInTurn: Int, totalDiceRolls: Int){
    def updateGameState(): GameState2 =
      if (player1Turn) {
        val newPosition = (pos1 + totalDiceRolls - 1) % 10 + 1
        GameState2(newPosition, pos2, points1 + newPosition, points2, player1Turn = false, 3, 0)
      }
      else {
        val newPosition = (pos2 + totalDiceRolls - 1) % 10 + 1
        GameState2(pos1, newPosition, points1, points2 + newPosition, player1Turn = true, 3, 0)
      }

    def rollDice(nextDiceRoll: Int): GameState2 =
      GameState2(pos1, pos2, points1, points2, player1Turn, rollsLeftInTurn-1, totalDiceRolls + nextDiceRoll)
  }

  case class Wins(p1Wins: Long, p2Wins: Long) {
    def max: Long = p1Wins.max(p2Wins)
  }

  def part2(pos1: Int, pos2: Int): Long = {

    val cache = scala.collection.mutable.Map.empty[GameState2, Wins]
    def countWins(gs: GameState2): Wins = {
      if (gs.points1 >= 21)
        Wins(1, 0)
      else if (gs.points2 >= 21)
        Wins(0, 1)
      else if (cache.contains(gs))
        cache(gs)
      else if (gs.rollsLeftInTurn == 0){
        countWins(gs.updateGameState())}
      else {
        cache(gs) = Range.inclusive(1,3)
          .map{x => countWins(gs.rollDice(x))}
          .foldLeft(Wins(0L, 0L)) {case (Wins(accA, accB), Wins(a, b)) => Wins(accA + a, accB + b)}
        cache(gs)
      }
    }

    countWins(GameState2(pos1, pos2, 0, 0, player1Turn = true, 3, 0)).max
  }

}
