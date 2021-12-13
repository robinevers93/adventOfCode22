package day13

import org.scalatest.wordspec.AnyWordSpec

class TransparentOrigamiTest extends AnyWordSpec{

  "TransparentOrigami" should {
    "find correct size and print a square" in {
      val input = general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day13/sampleData.txt")
      val coordinates = TransparentOrigami.parseInput(input)

      val foldY: Set[(Int, Int)] = TransparentOrigami.foldAlongY(coordinates, 7)
      val foldX: Set[(Int, Int)] = TransparentOrigami.foldAlongX(foldY, 5)

      assert(coordinates.size == 18)
      assert(foldY.size == 17)
      assert(foldX.size == 16)

      TransparentOrigami.printableAnswer(foldX).foreach(println(_))
    }

    "give puzzle answer" in {
      val input = general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day13/input.txt")
      val coordinates = TransparentOrigami.parseInput(input)
      val part1: Set[(Int, Int)] = TransparentOrigami.foldAlongX(coordinates, 655)
      println(part1.size)

      val instructions = TransparentOrigami.getInstructions(general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day13/instructions.txt"))
      val part2 = TransparentOrigami.executeInstructions(coordinates, instructions)
      TransparentOrigami.printableAnswer(part2).foreach(println(_))
    }

  }

}
