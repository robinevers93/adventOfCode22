package day19

import scala.annotation.tailrec
import scala.math.abs

object BeaconScanner {

  case class Coordinate(x: Int, y: Int, z: Int) {
    def +(other: Coordinate): Coordinate = Coordinate(x + other.x, y + other.y, z + other.z)
    def -(other: Coordinate): Coordinate = Coordinate(x - other.x, y - other.y, z - other.z)
    def permutations: Seq[Coordinate] = Seq(
      Coordinate(x, y, z), Coordinate(y, z, x), Coordinate(z, x, y), Coordinate(-x, z, y),
      Coordinate(z, y, -x), Coordinate(y, -x, z), Coordinate(x, z, -y), Coordinate(z, -y, x),
      Coordinate(-y, x, z), Coordinate(x, -z, y), Coordinate(-z, y, x), Coordinate(y, x, -z),
      Coordinate(-x, -y, z), Coordinate(-y, z, -x), Coordinate(z, -x, -y), Coordinate(-x, y, -z),
      Coordinate(y, -z, -x), Coordinate(-z, -x, y), Coordinate(x, -y, -z), Coordinate(-y, -z, x),
      Coordinate(-z, x, -y), Coordinate(-x, -z, -y), Coordinate(-z, -y, -x), Coordinate(-y, -x, -z))
    def manhattanDistance(that: Coordinate): Int = abs(x - that.x) + abs(y - that.y) + abs(z - that.z)
  }

  type Scanner = Set[Coordinate]
  def scannerPermutations(scanner: Scanner): Seq[Scanner] =
    scanner.toSeq.map(_.permutations).transpose.map(_.toSet)

  @tailrec
  def parseScanners(input: LazyList[String], numberOfScanners: Int, scanners: List[List[Coordinate]] = List()): List[Scanner] =
    if (scanners.length < numberOfScanners) {
      val scanner = input.takeWhile(x => !x.startsWith("---")).toList.map{case s"$x,$y,$z" =>  Coordinate(x.toInt,y.toInt,z.toInt)}
      parseScanners(input.drop(scanner.size + 1), numberOfScanners, scanner :: scanners)
    } else
      scanners.map(_.toSet)

  def mapBeaconsForScannerToDistanceFromOtherScanner(scanner1: Scanner, scanner2: Scanner): Map[Coordinate, Int] = (for {
    p1 <- scanner1.toList
    p2 <- scanner2.toList
  } yield p1 - p2).groupMapReduce(identity)(_ => 1)(_ + _)

  def scannersOverlapOn12Points(scanner1: Scanner, scanner2: Scanner): Option[(Scanner, Coordinate)] = (for {
    permutedScanner <- scannerPermutations(scanner2)
    (distance, count) <- mapBeaconsForScannerToDistanceFromOtherScanner(scanner1, permutedScanner)
    if count >= 12
  } yield (permutedScanner.map(_ + distance), distance)).headOption

  @tailrec
  def findIndividualBeacons(scanners: List[Scanner], acc: Set[Coordinate], scannerPositions: List[Coordinate]): (Set[Coordinate], List[Coordinate]) =
    if (scanners.isEmpty)
      (acc, scannerPositions)
    else {
      val (matchedScanners, permutedScanners, matchedScannerPositions) =
        (for {
          scanner <- scanners
          (permutedScanner, scannerPosition) <- scannersOverlapOn12Points(acc, scanner)
        } yield (scanner, permutedScanner, scannerPosition)).unzip3
      val newScannerPositions = scannerPositions ++ matchedScannerPositions
      findIndividualBeacons(scanners.filterNot(matchedScanners.contains), acc ++ permutedScanners.flatten, newScannerPositions)
    }

  def getLargestManhattanDistance(coordinates: List[Coordinate]): Int = {
    @tailrec
    def go(coordinates: List[Coordinate], maxDistance: Int): Int = coordinates match {
      case Nil =>
        maxDistance
      case _ :: Nil =>
        maxDistance
      case h :: t =>
        go(t, t.map(h.manhattanDistance).max.max(maxDistance))
    }
    go(coordinates.combinations(2).flatten.toList, 0)
  }

}
