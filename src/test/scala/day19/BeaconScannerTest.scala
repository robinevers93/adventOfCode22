package day19

import day19.BeaconScanner.{findIndividualBeacons, getLargestManhattanDistance, parseScanners}
import org.scalatest.wordspec.AnyWordSpec


class BeaconScannerTest extends AnyWordSpec{

  "BeaconScanner" should {
    "work" in {

      val input = general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day19/input.txt")
        .filterNot(_ == "")
        .to(LazyList)

      val scanners = parseScanners(input.tail, 26)
      val (beacons, scannerPositions) = findIndividualBeacons(scanners.tail, scanners.head, List())

      println(beacons.size)
      println(getLargestManhattanDistance(scannerPositions))

    }
  }

}
