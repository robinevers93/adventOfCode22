package day20

import day20.TrenchMap.updateGridN
import org.scalatest.wordspec.AnyWordSpec
import utils.Node

class TrenchMapTest extends AnyWordSpec{

  "Trench Map" should{

    val imageEnhancementAlgorithm = "###.#..##.####.....#.#..##.#....##.##.##..#.####.#.#.#.#.#..##.####.####...##.#.###..#..#.##...##..##..#....###########..####..###....#.##...##...#....#########.#..#..##..#..#.#.#.##...###..####.##.#####.####.#....#.#.##..###...#..#...##.##.#..#...#..##..#..#...###........#..##....#.#.##.##.##.######..##.#...#..#######.###.#..#.#....#.###.#....#.....#.#..##.#.......##..#..#..#...#..##.######.#.####.#....#.....#.##.#.#..##.#..##..#.##..#.##...###......##.#####..##.###.#.#.######.#####..#.#..#.....#....#.##.."
      .toList

    val startingGrid = general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day20/input.txt")

    val matrix = Node.matrixToGridMap(startingGrid)
    val expandedMatrix = Node.fillMap(matrix, startingGrid.size, 3, '.')

    "solve part 1" in {
      val newGrid = updateGridN(2, expandedMatrix, imageEnhancementAlgorithm)
      val part1 = newGrid.values.count(_ == '#')

      assert(part1 == 5347)
    }

    "solve part 2" in {
      val newGrid2 = updateGridN(50, expandedMatrix, imageEnhancementAlgorithm)
      val part2 = newGrid2.values.count(_ == '#')

      assert(part2 == 17172)
    }
  }

}
