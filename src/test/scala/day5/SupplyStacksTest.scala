package day5

import org.scalatest.wordspec.AnyWordSpec
import SupplyStacks.Instruction

class SupplyStacksTest extends AnyWordSpec {

  private val input: List[String] = general.ReadInTestData.getPuzzleInputAsList("src/test/scala/day5/sampleInstructions.txt")("\n")
  private val instructions: List[Instruction] = SupplyStacks.readInstructions(input)
  private val input2: List[String] = general.ReadInTestData.getPuzzleInputAsList("src/test/scala/day5/sampleData.txt")("\n")
  private val initialState: List[List[Char]] = SupplyStacks.readInitialState(input2)

  "Reading instructions" should {
    "be successful" in {
      assert(instructions == List(Instruction(1, 1, 0), Instruction(3, 0, 2), Instruction(2, 1, 0), Instruction(1, 0, 1)))
    }
  }

  "Creating initial state" should {
    "be successful" in {
      assert(initialState == List(List('N', 'Z'), List('D', 'C', 'M'), List('P')))
    }
  }

  "Updating state" should {
    "show the results for one round" in {
      val afterOneRound = SupplyStacks.executeInstruction(initialState, instructions.head, true)
      assert(afterOneRound == List(List('D', 'N', 'Z'), List('C', 'M'), List('P')))
    }
    "show the final result" in {
      val finalState = SupplyStacks.executeInstructions(initialState, instructions, true)
      assert(finalState.flatMap(_.headOption).mkString("") == "CMZ")
    }
  }

  "Updating state without reversing" should {
    "show the results for one round" in {
      val afterOneRound = SupplyStacks.executeInstruction(initialState, instructions.head, false)
      assert(afterOneRound == List(List('D', 'N', 'Z'), List('C', 'M'), List('P')))
    }
    "show the final result" in {
      val finalState = SupplyStacks.executeInstructions(initialState, instructions, false)
      assert(finalState.flatMap(_.headOption).mkString("") == "MCD")
    }
  }

  "The answer" should {
    "be correct" in {
      val input: List[String] = general.ReadInTestData.getPuzzleInputAsList("src/test/scala/day5/instructions.txt")("\n")
      val instructions: List[Instruction] = SupplyStacks.readInstructions(input)
      val input2: List[String] = general.ReadInTestData.getPuzzleInputAsList("src/test/scala/day5/input.txt")("\n")
      val initialState: List[List[Char]] = SupplyStacks.readInitialState(input2)

      val finalStateWithReverse = SupplyStacks.executeInstructions(initialState, instructions, true)
      val finalStateWithoutReverse = SupplyStacks.executeInstructions(initialState, instructions, false)

      assert(finalStateWithReverse.flatMap(_.headOption).mkString("") == "NTWZZWHFV")
      assert(finalStateWithoutReverse.flatMap(_.headOption).mkString("") == "BRZGFVBTJ")

    }
  }

}
