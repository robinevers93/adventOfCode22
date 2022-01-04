package utils

import scala.util.Try

case class PointWithHeight(x: Int, y: Int, height: Int){

  type Grid = Vector[Vector[PointWithHeight]]

  def getAdjacents(grid: Grid): Set[PointWithHeight] =
    Set(Try(grid(x - 1)(y)).toOption,
      Try(grid(x + 1)(y)).toOption,
      Try(grid(x)(y - 1)).toOption,
      Try(grid(x)(y + 1)).toOption).flatten
}
