package day1

import org.scalatest.wordspec.AnyWordSpec

class SonarSweepTest extends AnyWordSpec {

  "numberOfIncreases" should {
    "count the number of times a depth measurement increases from the previous measurement correctly" in {
      val input = general.ReadInTestData.getPuzzleInput(_.toInt)("src/test/scala/day1/sampleData.txt")
      val answer = SonarSweep.numberOfIncreases(input)
      assert(answer == 7)
    }

    "give the answer to the puzzle" in {
      val input = general.ReadInTestData.getPuzzleInput(_.toInt)("src/test/scala/day1/input.txt")
      val answer = SonarSweep.numberOfIncreases(input)
      println(answer)
    }
  }

  "measurementsAsThreeMeasurementWindow" should {

    "count the number of times a depth measurement for a sliding window of size three increases correctly" in {
      val input = general.ReadInTestData.getPuzzleInput(_.toInt)("src/test/scala/day1/sampleData.txt")

      val threeMeasurementWindows = SonarSweep.measurementsAsThreeMeasurementWindow(input)
      assert(threeMeasurementWindows == List(607, 618, 618, 617, 647, 716, 769, 792))

      val answer = SonarSweep.numberOfIncreases(threeMeasurementWindows)
      assert(answer == 5)
    }

    "give the answer to the puzzle" in {
      val input = general.ReadInTestData.getPuzzleInput(_.toInt)("src/test/scala/day1/input.txt")
      val answer = SonarSweep.numberOfIncreases(SonarSweep.measurementsAsThreeMeasurementWindow(input))
      println(answer)
    }
  }

}
