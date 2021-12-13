package day11

import day11.DumboOctopus.Point
import org.scalatest.wordspec.AnyWordSpec

class DumboOctopusTest extends AnyWordSpec{

  "Dumbo Octopus" should {
    "correctly get adjacents" in {
      val input: Vector[Vector[Int]] = general.ReadInTestData.getPuzzleInputAsVector(_.toVector)("src/test/scala/day11/sampleData.txt")
        .map(_.map(_.toInt-48))

      val points: Vector[Point] = input.zipWithIndex.flatMap(x => x._1.zipWithIndex.map(y => Point(x._2, y._2, y._1)))

      assert(Point(0,0,5).adjacents(points) == Set(Point(0,0,5), Point(0,1,4), Point(1,0,2), Point(1,1,7)))
      assert(Point(0,9,3).adjacents(points ) == Set(Point(0,9,3), Point(1,9,1), Point(0,8,2), Point(1,8,1)))
      assert(Point(9,0,5).adjacents(points ) == Set(Point(9,0,5),Point(9,1,2), Point(8,0,4), Point(8,1,8)))
    }

    "correctly update matrix" in {
      val input: Vector[Vector[Int]] = general.ReadInTestData.getPuzzleInputAsVector(_.toVector)("src/test/scala/day11/sampleData.txt")
        .map(_.map(_.toInt-48))

      val points: Vector[Point] = input.zipWithIndex.flatMap(x => x._1.zipWithIndex.map(y => Point(x._2, y._2, y._1)))

      val numberOfFlashes = DumboOctopus.numberOfFlashesAfterN(points, 101)
      assert(numberOfFlashes == 1656)

      val updatesUntilBright = DumboOctopus.increaseEnergyUntilBright(points)
      assert(updatesUntilBright == 195)

    }



  }

}
