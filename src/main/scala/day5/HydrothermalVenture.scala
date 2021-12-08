package day5

import scala.util.matching.Regex

object HydrothermalVenture {

  private val regex: Regex = """(\d+),(\d+) -> (\d+),(\d+)""".r

  def getRange(start: Int, end: Int): List[Int] =
    if (start < end)
      Range.inclusive(start, end).toList
    else
      Range.inclusive(start, end, -1).toList

  def lineToCoordinates(line: String): List[(Int, Int)] = line match {
    case regex(x1,y1,x2,y2) if x1 == x2 => getRange(y1.toInt, y2.toInt).map(y => (x1.toInt, y))
    case regex(x1,y1,x2,y2) if y1 == y2 => getRange(x1.toInt, x2.toInt).map(x => (x, y1.toInt))
    case regex(x1,y1,x2,y2) if x1.toInt < x2.toInt => getRange(x1.toInt, x2.toInt).zip(getRange(y1.toInt, y2.toInt))
    case regex(x1,y1,x2,y2) => getRange(x2.toInt, x1.toInt).zip(getRange(y2.toInt, y1.toInt))
    case _ => List()
  }

  def allPoints(lines: List[String]): List[(Int, Int)] =
    lines.flatMap(lineToCoordinates)

  def numberOfOverlappingPoints(lines: List[String]): Int =
    allPoints(lines).groupBy(identity).values.map(_.size).count(_ > 1)

}
