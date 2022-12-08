package day6

import org.scalatest.wordspec.AnyWordSpec

class TuningTroubleTest extends AnyWordSpec {

  private val input: List[Char] = general.ReadInTestData.getPuzzleInput("src/test/scala/day6/input.txt").toList

  "Solving for test input" should {
    "be successful" in {
      val testInput = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg".toList

      assert(TuningTrouble.solve(testInput, 4).contains(10))
      assert(TuningTrouble.solve(testInput, 14).contains(29))}
  }

  "Calculating answer to puzzle" should {
    "be successful" in {
      val answer1 = TuningTrouble.solve(input, 4)
      val answer2 = TuningTrouble.solve(input, 14)
      assert(answer1.contains(1198))
      assert(answer2.contains(3120))
    }
  }

}
