package day6

import org.scalatest.wordspec.AnyWordSpec

class LanternfishTest extends AnyWordSpec{

  "Calculating toal number of fish" should {
    "give the correct answer on sample data" in {
      val input = general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day6/sampleData.txt")
        .head.split(",").toList.map(_.toInt)

      val answer = Lanternfish.afterNDays(input, 256)
      assert(answer == 26984457539L)
    }

    "find the answer to the puzzle" in {
      val input = general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day6/input.txt")
        .head.split(",").toList.map(_.toInt)

      val answer = Lanternfish.afterNDays(input, 256)
      println(answer)
    }
  }

}
