package day2

import org.scalatest.wordspec.AnyWordSpec

class DiveTest extends AnyWordSpec {

  "getPosition" should {
    "calculate the new position correctly" in {
      val input = general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day2/sampleData.txt")
      val answer = Dive.getPosition(input, (0,0,0))
      assert(answer == 900)
    }

    "give the answer to the puzzle" in {
      val input = general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day2/input.txt")
      val answer = Dive.getPosition(input, (0,0,0))
      println(answer)
    }
  }


}
