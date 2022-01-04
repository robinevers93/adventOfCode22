package day24

import day24.ArithmeticLogicUnit.executeInstructions
import org.scalatest.wordspec.AnyWordSpec


class ArithmeticLogicUnitTest extends AnyWordSpec{

  "ArithmeticLogicUnit" should {

    "execute the instructions and pint the resulting ArithmeticLogicUnit" in {

      val testInstructions1 = List(
        "inp x",
        "mul x -1"
      )

      val testInstructions2 = List(
        "inp z",
        "inp x",
        "mul z 3",
        "eql z x"
      )

      val alu1 = executeInstructions(testInstructions1, List(1))
      assert(alu1('x') == -1)

      val alu2 = executeInstructions(testInstructions2, List(1, 2))
      assert(alu2('z') == 0)

      val alu3 = executeInstructions(testInstructions2, List(3, 9))
      assert(alu3('z') == 1)

      val instructions = general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day24/input.txt")

      val largestModelNumber = List(9,9,9,1,9,6,9,2,4,9,6,9,3,9)
      assert(executeInstructions(instructions, largestModelNumber)('z') == 0)
      println(largestModelNumber.mkString(""))

      val smallestModelNumber = List(8,1,9,1,4,1,1,1,1,6,1,7,1,4)
      assert(executeInstructions(instructions, smallestModelNumber)('z') == 0)
      println(smallestModelNumber.mkString(""))

    }
  }

}
