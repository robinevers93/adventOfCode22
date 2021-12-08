package day7

import org.scalatest.wordspec.AnyWordSpec

class WhalesTest extends AnyWordSpec{

  "Getting fuel cost" should {
    "give correct results" in {
      val input = general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day7/sampleData.txt")
        .head.split(",").toList.map(_.toInt)

      assert(Whales.fuelCost(input, 2) == 37)
      assert(Whales.fuelCost(input, 1) == 41)
      assert(Whales.fuelCost(input, 3) == 39)
      assert(Whales.fuelCost(input, 10) == 71)
      assert(Whales.fuelRequired(input, Whales.fuelCost) == 37)

      assert(Whales.fuelCost2(input, 2) == 206)
      assert(Whales.fuelRequired(input, Whales.fuelCost2) == 168)
    }

    "give answer to the puzzle" in {
      val input = general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day7/input.txt")
        .head.split(",").toList.map(_.toInt)

      println(Whales.fuelRequired(input, Whales.fuelCost))
      println(Whales.fuelRequired(input, Whales.fuelCost2))

      println(Whales.fuelCost2(input, 473))
    }
  }

  "solve puzzle with maths" should {
    "be more fun" in {
      val input = general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day7/sampleData.txt")
        .head.split(",").toList.map(_.toInt)

      val optimalPositionPt1 = Whales.optimalPosition(input, Whales.f)
      val optimalPositionPt2 = Whales.optimalPosition(input, Whales.f2)

      assert(optimalPositionPt1 == 2)
      assert(optimalPositionPt2 == 5)
    }
  }



}
