package day11

import zio.Runtime
import zio.stream.Stream

import scala.annotation.tailrec

object DumboOctopus {

  case class Point(x: Int, y: Int, energyLevel: Int){
    def placeInList: Int = y + 10*x

    def adjacents(grid: Vector[Point]): Set[Point] = {
      val horizontalRange = Range.inclusive(0.max(x - 1), 9.min(x + 1))
      val verticalRange = Range.inclusive(0.max(y - 1), 9.min(y + 1))
      (for {
        xCor <- horizontalRange
        yCor <- verticalRange
      } yield grid.find(point => point.x == xCor && point.y == yCor)).toSet.flatten
    }
  }

  def updatePoint(point: Point, adjacents: Set[Point], flashes: Set[Point]): Point =
    if (flashes.contains(point))
      point.copy(energyLevel = 0)
    else
      point.copy(energyLevel = point.energyLevel + adjacents.count(flashes.contains))

  @tailrec
  def getSubsequentFlashes(grid: Vector[Point], acc: Set[Point]): Set[Point] =
    acc.flatMap(_.adjacents(grid)).filter { point =>
      grid(point.placeInList).energyLevel + point.adjacents(grid).intersect(acc).size > 9
    } match {
      case flashes if flashes == acc => flashes
      case flashes => getSubsequentFlashes(grid, flashes)
    }

  def increaseEnergyLevels(grid: Vector[Point], numberOfFlashes: Int): (Vector[Point], Int) = {
    val newGrid = grid.map(p => p.copy(energyLevel = p.energyLevel + 1))
    val initialFlashes = newGrid.filter(_.energyLevel > 9).toSet
    val allFlashes = getSubsequentFlashes(newGrid, initialFlashes)
    (newGrid.map(p => updatePoint(p, p.adjacents(newGrid), allFlashes)), numberOfFlashes + allFlashes.size)
  }

  def numberOfFlashesAfterN(grid: Vector[Point], n: Long): Int =
    Runtime.default.unsafeRun(
      Stream.iterate((grid,0)){ case (grid, flashes) => increaseEnergyLevels(grid, flashes) }
        .take(n)
        .runLast
        .someOrFail("Boom!")
    )._2

  def increaseEnergyUntilBright(grid: Vector[Point]): Long =
    Runtime.default.unsafeRun(
      Stream.iterate((grid,0)){ case (grid, flashes) => increaseEnergyLevels(grid, flashes) }
        .takeWhile{ case (grid, _) => grid.map(_.energyLevel) != List.fill(100)(0) }
        .runCount
    )
}
