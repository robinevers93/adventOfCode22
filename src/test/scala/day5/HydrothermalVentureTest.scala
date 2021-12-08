package day5

import org.scalatest.wordspec.AnyWordSpec

class HydrothermalVentureTest extends AnyWordSpec{

  "numberOfOverlappingPoints" should {

    "deal with straights" in {
      val test = "1,1 -> 1,3"
      val points = HydrothermalVenture.allPoints(List(test))
      assert(points == List((1,1),(1,2),(1,3)))
    }

    "deal with diagonals" in {
      val test = "1,1 -> 3,3"
      val points = HydrothermalVenture.allPoints(List(test))
      assert(points == List((1,1),(2,2),(3,3)))

      val test2 = "3,3 -> 1,1"
      val points2 = HydrothermalVenture.allPoints(List(test2))
      assert(points2 == List((1,1),(2,2),(3,3)))

      val test3 = "8,0 -> 0,8"

      val points3 = HydrothermalVenture.allPoints(List(test3))
      assert(points3 == List((0,8),(1,7),(2,6),(3,5),(4,4),(5,3),(6,2),(7,1),(8,0)))
    }

    "find the number of overlapping points" in {
      val input = general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day5/sampleData.txt")
      val answer = HydrothermalVenture.numberOfOverlappingPoints(input)
      assert(answer == 12)
    }

    "give the answer to the puzzle" in {
      val input = general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day5/input.txt")
      val answer = HydrothermalVenture.numberOfOverlappingPoints(input)
      assert(answer == 20373 )
    }

  }

}
