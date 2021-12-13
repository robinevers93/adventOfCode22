package day12

import day12.PassagePathing.CaveMapping
import org.scalatest.wordspec.AnyWordSpec

class PassagePathingTest extends AnyWordSpec{

  "PassagePathing" should {
    "correctly parse coordinates" in {
      val input = general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day12/sampleData.txt")
      val caveMapping: CaveMapping = PassagePathing.inputToCaveMapping(input)

      val expectedCaveMapping = Map(
        "start" -> Set("A", "b"),
        "A" -> Set("start", "b", "c", "end"),
        "b" -> Set("start", "A", "d", "end"),
        "c" -> Set("A"),
        "d" -> Set("b"),
        "end" -> Set("A", "b"),
      )

      assert(caveMapping == expectedCaveMapping)
      assert(caveMapping("start") == Set("A", "b"))
    }

    "find paths from start to end" in {
      val input = general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day12/sampleData.txt")
      val answer = general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day12/sampleAnswer.txt")
        .map(_.split(",").reverse.toList).toSet

      val caveMapping: CaveMapping = PassagePathing.inputToCaveMapping(input)
      val part1 = PassagePathing.findPathsFromStart(caveMapping, false)
      val part2 = PassagePathing.findPathsFromStart(caveMapping, true)

      assert(part1.size == 10)
      assert(part2 == answer)
      assert(part2.size == 36)
    }
  }

}
