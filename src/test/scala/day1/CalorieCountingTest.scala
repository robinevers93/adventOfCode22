package day1

import org.scalatest.wordspec.AnyWordSpec

class CalorieCountingTest extends AnyWordSpec {

  "this number" should {
    "be the maximum sum of calories carried by an elf" in {
      val input = general.ReadInTestData.getListsPuzzleInputAsFormattedLists(_.toInt)("src/test/scala/day1/sampleData.txt")("\n\n")
      val answer = CalorieCounting.maxCalories(input)
      assert(answer == 24000)
    }

    "give the answer to puzzle 1" in {
      val input = general.ReadInTestData.getListsPuzzleInputAsFormattedLists(_.toInt)("src/test/scala/day1/input.txt")("\n\n")
      val answer = CalorieCounting.maxCalories(input)
      println(answer)
    }
  }

  "this number " should {
    "be sum of calories carried the 3 elves carrying the most calories" in {
      val input = general.ReadInTestData.getListsPuzzleInputAsFormattedLists(_.toInt)("src/test/scala/day1/sampleData.txt")("\n\n")
      val answer = CalorieCounting.totalCaloriesMax3(input)
      assert(answer == 45000)
    }

    "give the answer to puzzle 2" in {
      val input = general.ReadInTestData.getListsPuzzleInputAsFormattedLists(_.toInt)("src/test/scala/day1/input.txt")("\n\n")
      val answer = CalorieCounting.totalCaloriesMax3(input)
      println(answer)
    }

  }

}
