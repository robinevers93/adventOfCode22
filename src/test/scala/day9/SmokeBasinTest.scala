package day9

import org.scalatest.wordspec.AnyWordSpec
import utils.PointWithHeight

class SmokeBasinTest extends AnyWordSpec{

  "Smoke Basin" should {
    "identify low points correctly" in {
      val input: Vector[Vector[Int]] = general.ReadInTestData.getPuzzleInputAsVector(_.toVector)("src/test/scala/day9/sampleData.txt")
        .map(_.map(_.toInt-48))

      val points: Vector[Vector[PointWithHeight]] = input.zipWithIndex.map(x => x._1.zipWithIndex.map(y => PointWithHeight(x._2, y._2, y._1)))

      assert(!SmokeBasin.isLowPoint(points, PointWithHeight(0, 0, 2)))
      assert(SmokeBasin.isLowPoint(points, PointWithHeight(0, 1, 1)))

    }
    "find risk correctly" in {
      val input: Vector[Vector[Int]] = general.ReadInTestData.getPuzzleInputAsVector(_.toVector)("src/test/scala/day9/sampleData.txt")
        .map(_.map(_.toInt-48))

      val points: Vector[Vector[PointWithHeight]] = input.zipWithIndex.map(x => x._1.zipWithIndex.map(y => PointWithHeight(x._2, y._2, y._1)))

      val test: Vector[PointWithHeight] = SmokeBasin.getLowPoints(points)
      val risk = SmokeBasin.getRisk(test)
      assert(risk == 15)
    }

    "find basin size" in {
      val input: Vector[Vector[Int]] = general.ReadInTestData.getPuzzleInputAsVector(_.toVector)("src/test/scala/day9/sampleData.txt")
        .map(_.map(_.toInt-48))

      val points = input.zipWithIndex.map(x => x._1.zipWithIndex.map(y => PointWithHeight(x._2, y._2, y._1)))

      val lowPoints = SmokeBasin.getLowPoints(points)

      val lowPoint = PointWithHeight(2,2,5)
      val lowPoint1 = PointWithHeight(0,1,1)
      val lowPoint2 = PointWithHeight(4,6,5)
      val lowPoint3 = PointWithHeight(0,9,0)

      assert(lowPoints.toSet == Set(lowPoint, lowPoint1, lowPoint2, lowPoint3))

      val adjacents = lowPoint1.getAdjacents(points)
      assert(adjacents == Set(PointWithHeight(1,1,9), PointWithHeight(0,0,2), PointWithHeight(0,2,9)))

      val basin = SmokeBasin.getBasin(points, lowPoint)
      assert(basin.size == 14)

      val answer = SmokeBasin.getLargestBasinSizes(points)
      assert(answer == 1134)
    }
  }

}
