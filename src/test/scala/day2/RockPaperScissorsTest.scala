package day2

import org.scalatest.wordspec.AnyWordSpec

class RockPaperScissorsTest extends AnyWordSpec {

  "this number" should {
    "be the maximum sum of calories carried by an elf" in {
      val input = general.ReadInTestData.getPuzzleInputAsFormattedList(RockPaperScissors.readPair)("src/test/scala/day2/sampleData.txt")("\n")
      val answer = RockPaperScissors.totalPoints(input)
      assert(answer == 15)
    }

    "give the answer to puzzle 1" in {
      val input = general.ReadInTestData.getPuzzleInputAsFormattedList(RockPaperScissors.readPair)("src/test/scala/day2/input.txt")("\n")
      val answer = RockPaperScissors.totalPoints(input)
      println(answer)
    }
  }

  "this number " should {
    "be sum of calories carried the 3 elves carrying the most calories" in {
      val input = general.ReadInTestData.getPuzzleInputAsFormattedList(RockPaperScissors.readPair2)("src/test/scala/day2/sampleData.txt")("\n")
      val moves = RockPaperScissors.toMoves(input)
      val answer = RockPaperScissors.totalPoints(moves)
      assert(answer == 12)
    }

    "give the answer to puzzle 2" in {
      val input = general.ReadInTestData.getPuzzleInputAsFormattedList(RockPaperScissors.readPair2)("src/test/scala/day2/input.txt")("\n")
      val moves = RockPaperScissors.toMoves(input)
      val answer = RockPaperScissors.totalPoints(moves)
      println(answer)
    }

  }

}
