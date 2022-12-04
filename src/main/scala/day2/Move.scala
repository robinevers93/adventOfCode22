package day2

enum Move(val score: Int) {
  case Rock extends Move(1)
  case Paper extends Move(2)
  case Scissors extends Move(3)

  def play(other: Move): Int = (this, other) match {
    case (Scissors, Rock) | (Rock, Paper) | (Paper, Scissors) => 6
    case _ if this == other => 3
    case _ => 0
  }
}

object Move {

  def readMove(char: Char): Move = char match {
    case 'A' | 'X' => Move.Rock
    case 'B' | 'Y' => Move.Paper
    case 'C' | 'Z' => Move.Scissors
  }
  
}