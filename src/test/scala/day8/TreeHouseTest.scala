package day8

import org.scalatest.wordspec.AnyWordSpec

class TreeHouseTest extends AnyWordSpec {

  val transform: String => List[Int] = (s: String) => s.toList.map(_.toInt - 48)
  private val testInput: List[List[Int]] = general.ReadInTestData.getPuzzleInputAsFormattedList(transform)("src/test/scala/day8/sampleData.txt")("\n")
  private val input: List[List[Int]] = general.ReadInTestData.getPuzzleInputAsFormattedList(transform)("src/test/scala/day8/input.txt")("\n")

  "Calculating the number of visible trees" should {
    "work" in {
      val outsideTrees = TreeHouse.outsideTrees(testInput)
      val insideTrees = TreeHouse.insideTrees(testInput)
      val totalVisibleTrees = outsideTrees + insideTrees

      assert(outsideTrees == 16)
      assert(insideTrees == 5)
      assert(totalVisibleTrees == 21)
    }

    "print puzzle answer" in {
      val outsideTrees = TreeHouse.outsideTrees(input)
      val insideTrees = TreeHouse.insideTrees(input)
      val totalVisibleTrees = outsideTrees + insideTrees

      println(totalVisibleTrees)

    }
  }

  "Calculating the max of visibility score" should {
    "work" in {
      val score = TreeHouse.maxScore(testInput)
      assert(score == 8)
    }

    "print puzzle answer" in {
      val score = TreeHouse.maxScore(input)
      println(score)

    }
  }

}
