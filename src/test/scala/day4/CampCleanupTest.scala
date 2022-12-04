package day4

import org.scalatest.wordspec.AnyWordSpec

class CampCleanupTest extends AnyWordSpec {

  "the number of contained assignments" should {
    "be 2" in {
      val input = general.ReadInTestData.getPuzzleInputAsFormattedList(CampCleanup.apply)("src/test/scala/day4/sampleData.txt")("\n")
      val answer = CampCleanup.numberOfContainedAssignments(input)
      assert(answer == 2)
    }

    "print the answer to the first puzzle" in {
      val input = general.ReadInTestData.getPuzzleInputAsFormattedList(CampCleanup.apply)("src/test/scala/day4/input.txt")("\n")
      val answer = CampCleanup.numberOfContainedAssignments(input)
      println(answer)
    }
  }

  "the number of overlapping assignments" should {
    "be 4" in {
      val input = general.ReadInTestData.getPuzzleInputAsFormattedList(CampCleanup.apply)("src/test/scala/day4/sampleData.txt")("\n")
      val answer = CampCleanup.numberOfOverlappingAssignments(input)
      assert(answer == 4)
    }

    "print the answer to the second puzzle" in {
      val input = general.ReadInTestData.getPuzzleInputAsFormattedList(CampCleanup.apply)("src/test/scala/day4/input.txt")("\n")
      val answer = CampCleanup.numberOfOverlappingAssignments(input)
      println(answer)
    }
  }


}
