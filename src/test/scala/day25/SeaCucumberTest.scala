package day25

import day25.SeaCucumber.moveUntilStuck
import org.scalatest.wordspec.AnyWordSpec
import utils.Node.matrixToGridMap

class SeaCucumberTest extends AnyWordSpec {

  "Sea cucumber" should {
    "solve for sample data" in {

      val matrix = List(
        "v...>>.vv>",
        ".vv>>.vv..",
        ">>.>v>...v",
        ">>v>>.>.v.",
        "v>v.vv.v..",
        ">.>>..v...",
        ".vv..>.>v.",
        "v.v..>>v.v",
        "....v..v.>"
      )

      val gridMap = matrixToGridMap(matrix)
      assert(moveUntilStuck(gridMap)._2 == 58)


    }
    "solve for input data" in {

      val matrix = general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day25/input.txt")
      val gridMap = matrixToGridMap(matrix)
      assert(moveUntilStuck(gridMap)._2 == 337)

    }
  }

}
