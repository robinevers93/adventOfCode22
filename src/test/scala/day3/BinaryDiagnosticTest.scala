package day3

import org.scalatest.wordspec.AnyWordSpec

class BinaryDiagnosticTest extends AnyWordSpec{

  "getPowerConsumption" should {
    "calculate the power consumption correctly" in {
      val input = general.ReadInTestData.getPuzzleInput(_.toList.map(_.toInt - 48))("src/test/scala/day3/sampleData.txt")
      val gammaRate = BinaryDiagnostic.getGammaRate(input)
      assert(gammaRate == List(1,0,1,1,0))
      val epsilonRate = BinaryDiagnostic.getEpsilonRate(gammaRate)
      assert(epsilonRate == List(0,1,0,0,1))
      val answer = BinaryDiagnostic.multiplyBinaryNumbers(gammaRate, epsilonRate)
      assert(answer == 198)
    }

    "give the answer to the puzzle" in {
      val input = general.ReadInTestData.getPuzzleInput(_.toList.map(_.toInt - 48))("src/test/scala/day3/input.txt")
      val gammaRate = BinaryDiagnostic.getGammaRate(input)
      val epsilonRate = BinaryDiagnostic.getEpsilonRate(gammaRate)
      val answer = BinaryDiagnostic.multiplyBinaryNumbers(gammaRate, epsilonRate)
      println(answer)
    }
  }

  "getLifeSupportRating" should {
    "calculate the life support rating correctly" in {
      val input = general.ReadInTestData.getPuzzleInput(_.toList.map(_.toInt - 48))("src/test/scala/day3/sampleData.txt")
      val oxygen = BinaryDiagnostic.filterList(input, 0, BinaryDiagnostic.oxygenCriteria)
      assert(oxygen == List(1, 0, 1, 1, 1))

      val co2 = BinaryDiagnostic.filterList(input, 0, BinaryDiagnostic.co2Criteria)
      assert(co2 == List(0, 1, 0, 1, 0))

      val answer = BinaryDiagnostic.multiplyBinaryNumbers(oxygen, co2)
      assert(answer == 230)
    }

    "give the answer to the puzzle" in {
      val input = general.ReadInTestData.getPuzzleInput(_.toList.map(_.toInt - 48))("src/test/scala/day3/input.txt")
      val oxygen = BinaryDiagnostic.filterList(input, 0, BinaryDiagnostic.oxygenCriteria)
      val co2 = BinaryDiagnostic.filterList(input, 0, BinaryDiagnostic.co2Criteria)

      val answer = BinaryDiagnostic.multiplyBinaryNumbers(oxygen, co2)
      println(answer)
    }
  }


}
