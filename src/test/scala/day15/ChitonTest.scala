package day15

import org.scalatest.wordspec.AnyWordSpec
import utils.Node
import utils.Node.GridMap

class ChitonTest extends AnyWordSpec{

  "Dijkstra" should {
    "solve part 1" in {

      val input = general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day15/input.txt")
      val gridMap: GridMap[Int] = Chiton.inputToGridMap(input)
      val gridSize = input.length

      val startNode = Node(0, 0)
      val endNode = Node(gridSize - 1, gridSize - 1)

      val part1 = new Chiton(gridMap).dijkstra(startNode, endNode)
      println(part1)
    }

    "solve part 2" in {
      val input = general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day15/input.txt")
      val gridMap: GridMap[Int] = Chiton.inputToGridMap(input)
      val gridSize = input.length

      val startNode = Node(0, 0)
      val endNode = Node(gridSize * 5 - 1, gridSize * 5 - 1)

      val expandedGridMap: GridMap[Int] = Chiton.getFullMap(gridMap, gridSize)
      val part2 = new Chiton(expandedGridMap).dijkstra(startNode, endNode)

      println(part2)
    }
  }


}
