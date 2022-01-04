package day22

import org.scalatest.wordspec.AnyWordSpec


class ReactorRebootTest extends AnyWordSpec{

  "part1" should {
    "work" in {

      val instructions = List(
        "on x=10..12,y=10..12,z=10..12",
        "on x=11..13,y=11..13,z=11..13",
        "off x=9..11,y=9..11,z=9..11",
        "on x=10..10,y=10..10,z=10..10")

      val cuboid = ReactorReboot.part1(instructions, Set())
      val answer = cuboid.size
      println(answer)

      val instructions2 = general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day22/sampleData.txt")
      val cuboid2 = ReactorReboot.part1(instructions2, Set())
      val answer2 = cuboid2.size
      println(answer2)
    }
  }

  "part2" should {
    "work" in {

      val instructions = List(
        "on x=10..12,y=10..12,z=10..12",
        "on x=11..13,y=11..13,z=11..13",
        "off x=9..11,y=9..11,z=9..11",
        "on x=10..10,y=10..10,z=10..10")

      val parsedInstructions = ReactorReboot.inputToInstructions(instructions)
      val cuboid = ReactorReboot.part2(parsedInstructions.toList)
      val answer = cuboid.map(_.size).sum
      println(answer)

      val instructions2 = general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day22/sampleData.txt")
      val parsedInstructions2 = ReactorReboot.inputToInstructions(instructions2)
      val answer2 = ReactorReboot.part2(parsedInstructions2.toList)

      val size = answer2.toSeq.map(_.size).sum
      println(size)

    }
  }

}

