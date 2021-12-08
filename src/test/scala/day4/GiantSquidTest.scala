package day4

import day4.GiantSquidTest.{inputInstructions, inputPuzzles, sampleDataInstructions, sampleDataPuzzles}
import org.scalatest.wordspec.AnyWordSpec

class GiantSquidTest extends AnyWordSpec{

  "GiantSquid" should {
    "find the winner, the number of turns and the score" in {
      val (numberOfTurns, winner) = GiantSquid.getWinner(sampleDataPuzzles, sampleDataInstructions)
      val score = GiantSquid.getScore(sampleDataPuzzles(winner), sampleDataInstructions.take(numberOfTurns))

      assert(numberOfTurns == 12)
      assert(winner == 2)
      assert(score == 4512)
    }

    "find answer to the puzzle" in {
      val (numberOfTurns, winner) = GiantSquid.getWinner(inputPuzzles, inputInstructions)
      val score = GiantSquid.getScore(inputPuzzles(winner), inputInstructions.take(numberOfTurns))
      println(score)
    }

    "find the board that wins last" in {
      val (numberOfTurns, loser) = GiantSquid.getLoser(sampleDataPuzzles, sampleDataInstructions)
      val score = GiantSquid.getScore(sampleDataPuzzles(loser), sampleDataInstructions.take(numberOfTurns))

      assert(loser == 1)
      assert(score == 1924)
    }

    "find the answer to the puzzle" in {
      val (numberOfTurns, loser) = GiantSquid.getLoser(inputPuzzles, inputInstructions)
      val score = GiantSquid.getScore(inputPuzzles(loser), inputInstructions.take(numberOfTurns))
      println(score)
    }
  }

}

object GiantSquidTest {

  val sampleDataInstructions: List[Int] = general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day4/sampleDataInstructions.txt")
    .head.split(",").map(_.toInt).toList

  val sampleDataPuzzles : List[List[List[Int]]] = general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day4/sampleData.txt")
    .map(_.split("(?<=\\G...)").toList)
    .grouped(6).toList
    .map(_.dropRight(1))
    .map(_.map(_.map(_.strip().toInt)))

  val inputInstructions: List[Int] = general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day4/inputInstructions.txt")
    .head.split(",").map(_.toInt).toList

  val inputPuzzles: List[List[List[Int]]] = general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day4/input.txt")
    .map(_.split("(?<=\\G...)").toList)
    .grouped(6).toList
    .map(_.dropRight(1))
    .map(_.map(_.map(_.strip().toInt)))

}
