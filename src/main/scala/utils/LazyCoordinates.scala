package utils

import day22.ReactorReboot.Coordinate
import utils.LazyCoordinates.{intersectOneDimensional, splitOneDimensional}

case class LazyCoordinates(x1: Int, x2: Int, y1: Int, y2: Int, z1: Int, z2: Int) {

  def toCoordinates: Set[Coordinate]= (for {
    x <- Range.inclusive(x1, x2)
    y <- Range.inclusive(y1, y2)
    z <- Range.inclusive(z1, z2)
  } yield Coordinate(x,y,z)).toSet

  def restrictRange(min: Int, max: Int): LazyCoordinates =
    LazyCoordinates(min.max(x1), max.min(x2), min.max(y1), max.min(y2), min.max(z1), max.min(z2))

  def size: BigInt = BigInt(x2 - x1 + 1) * BigInt(y2 - y1 + 1) * BigInt(z2 - z1 + 1)

  def intersect(that: LazyCoordinates): Option[LazyCoordinates] = for {
    (xx1, xx2) <- intersectOneDimensional((x1,x2), (that.x1, that.x2))
    (yy1, yy2) <- intersectOneDimensional((y1,y2), (that.y1, that.y2))
    (zz1, zz2) <- intersectOneDimensional((z1,z2), (that.z1, that.z2))
  } yield LazyCoordinates(xx1, xx2, yy1, yy2, zz1, zz2)

  def split(that: LazyCoordinates): List[LazyCoordinates] = {
    (splitOneDimensional((x1,x2), (that.x1, that.x2)),
      splitOneDimensional((y1,y2), (that.y1, that.y2)),
      splitOneDimensional((z1,z2), (that.z1, that.z2))) match {
      case ((xx1, xx2):: xst, (yy1, yy2)::yst, _::zst) =>
        xst.map { case (x1, x2) => LazyCoordinates(x1, x2, y1, y2, z1, z2) } ++
          yst.map{ case (y1, y2) => LazyCoordinates(xx1, xx2, y1, y2, z1, z2)} ++
          zst.map{ case (z1, z2) => LazyCoordinates(xx1, xx2, yy1, yy2, z1, z2)}
      case _ =>
        List()
    }

  }

}

object LazyCoordinates {

  def intersectOneDimensional(oldRange: (Int, Int), newRange: (Int, Int)): Option[(Int, Int)] = (oldRange, newRange) match {
    case ((oldStart, oldEnd),(newStart, newEnd)) =>
      val (min, max) = (oldStart.max(newStart), oldEnd.min(newEnd))
      if (min > max)
        None
      else
        Some((min, max))
  }

  def splitOneDimensional(oldRange: (Int, Int), newRange: (Int, Int)): List[(Int, Int)] = (oldRange, newRange) match {
    case ((oldStart, oldEnd),(newStart, newEnd)) if newStart <= oldStart && newEnd >= oldEnd =>
      List(oldRange)
    case ((oldStart, oldEnd),(newStart, newEnd)) if oldStart < newStart && newEnd < oldEnd =>
      List( (newStart, newEnd), (oldStart, newStart -1), (newEnd + 1, oldEnd))
    case ((oldStart, oldEnd),(_, newEnd)) if oldStart <= newEnd && newEnd < oldEnd =>
      List((oldStart, newEnd), (newEnd + 1, oldEnd))
    case ((oldStart, oldEnd), (newStart, newEnd)) if oldStart < newStart && newStart <= oldEnd =>
      List((newStart, newEnd), (oldStart, newStart - 1))
    case _ =>
      List()
  }
}
