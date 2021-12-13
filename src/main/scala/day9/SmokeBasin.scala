package day9

import scala.annotation.tailrec
import scala.util.Try

object SmokeBasin {

  case class Point(x: Int, y: Int, height: Int)

  def getAdjacents(input: Vector[Vector[Point]], point: Point): Set[Point] =
    Set(Try(input(point.x - 1)(point.y)).toOption,
      Try(input(point.x + 1)(point.y)).toOption,
      Try(input(point.x)(point.y - 1)).toOption,
      Try(input(point.x)(point.y + 1)).toOption).flatten

  def isLowPoint(input: Vector[Vector[Point]], p: Point): Boolean =
    !getAdjacents(input, p).exists(_.height <= p.height)

  def getLowPoints(input: Vector[Vector[Point]]): Vector[Point] =
    input.flatten.filter(isLowPoint(input, _))

  def getRisk(points: Vector[Point]): Int =
    points.map(x => x.height+1).sum

  def getBasin(input: Vector[Vector[Point]], lowPoint: Point): Set[Point] = {
    @tailrec
    def findBasin(startingPoint: Point, basin: Set[Point], adjacents: Set[Point]): Set[Point] = {
      val newAdjacents = getAdjacents(input, startingPoint).filter(_.height < 9) -- basin ++ adjacents
      newAdjacents.headOption match {
        case Some(a) => findBasin(a, basin, newAdjacents.tail)
        case _ => basin + startingPoint
      }
    }
    findBasin(lowPoint, Set(), getAdjacents(input, lowPoint).filter(_.height < 9))
  }

  def getLargestBasinSizes(input: Vector[Vector[Point]]): Int = {
    getLowPoints(input).map(getBasin(input, _).size).sortBy(- _).take(3).product
  }

}
