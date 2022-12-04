package day2

import day2.Move.*

enum Result(val score: Int) {
  case Win extends Result(3)
  case Draw extends Result(2)
  case Lose extends Result(1)
}

object Result {

  def readResult(char: Char): Result = char match {
    case 'X' => Result.Lose
    case 'Y' => Result.Draw
    case 'Z' => Result.Win
  }
}