package day2

import day2.Move.*
import day2.Result.*

object RockPaperScissors {
  
  def readPair(string: String): (Move, Move) = {
    val Array(a, b) = string.split(" ")
    (readMove(a.head), readMove(b.head))
  }
  
  def readPair2(string: String): (Move, Result) = {
    val Array(a, b) = string.split(" ")
    (readMove(a.head), readResult(b.head))
  }

  def getMove(opponentMove: Move, result: Result): Move = (opponentMove, result) match {
    case (move: Move, Result.Draw) => move
    case (Move.Rock, Result.Lose) => Move.Scissors
    case (Move.Rock, Result.Win) => Move.Paper
    case (Move.Paper, Result.Lose) => Move.Rock
    case (Move.Paper, Result.Win) => Move.Scissors
    case (Move.Scissors, Result.Lose) => Move.Paper
    case (Move.Scissors, Result.Win) => Move.Rock
  }

  def totalPoints(moves: List[(Move, Move)]): Int = moves.map { case (a, b) => a.play(b) + b.score }.sum

  def toMoves(rounds: List[(Move, Result)]): List[(Move, Move)] = rounds.map { case (move, result) => (move, getMove(move, result)) }

}
