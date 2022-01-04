package day14

import org.scalatest.wordspec.AnyWordSpec

class ExtendedPolymerizationTest extends AnyWordSpec{

  "ExtendedPolymerization" should {
    "part 1" in {
      val input = ExtendedPolymerization.parseRules(
        general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day14/input.txt"))

      val after10 = ExtendedPolymerization.polymeriseN("OOBFPNOPBHKCCVHOBCSO".toList, input, 40)
      val score = ExtendedPolymerization.getScore(after10)
      println(score)
    }

    "part 2" in {
      val input = general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day14/input.txt")
      val part2 = ExtendedPolymerization2("OOBFPNOPBHKCCVHOBCSO", input).getAnswer

      println(part2)
    }
  }

}
