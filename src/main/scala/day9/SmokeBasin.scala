package day9

import utils.PointWithHeight

import scala.annotation.tailrec

object SmokeBasin {

  def isLowPoint(input: Vector[Vector[PointWithHeight]], p: PointWithHeight): Boolean =
    !p.getAdjacents(input).exists(_.height <= p.height)

  def getLowPoints(input: Vector[Vector[PointWithHeight]]): Vector[PointWithHeight] =
    input.flatten.filter(isLowPoint(input, _))

  def getRisk(points: Vector[PointWithHeight]): Int =
    points.map(x => x.height+1).sum

  def getBasin(input: Vector[Vector[PointWithHeight]], lowPoint: PointWithHeight): Set[PointWithHeight] = {
    @tailrec
    def findBasin(startingPoint: PointWithHeight, basin: Set[PointWithHeight], adjacents: Set[PointWithHeight]): Set[PointWithHeight] = {
      val newAdjacents = startingPoint.getAdjacents(input).filter(_.height < 9) -- basin ++ adjacents
      newAdjacents.headOption match {
        case Some(a) => findBasin(a, basin, newAdjacents.tail)
        case _ => basin + startingPoint
      }
    }
    findBasin(lowPoint, Set(), lowPoint.getAdjacents(input).filter(_.height < 9))
  }

  def getLargestBasinSizes(input: Vector[Vector[PointWithHeight]]): Int = {
    getLowPoints(input).map(getBasin(input, _).size).sortBy(- _).take(3).product
  }

}
