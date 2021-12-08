package day8

import org.scalatest.wordspec.AnyWordSpec

class SevenSegmentSearchTest extends AnyWordSpec{

  "decipherDigits" should {
    "should work successfully" in {
      val (signalPatterns, outputValues) = SevenSegmentSearchTest.getInput("src/test/scala/day8/sampleData.txt")

      val numberOfEasyDigits = outputValues.map(SevenSegmentSearch.getNumberOfEasyDigits).sum
      val deciphers = signalPatterns.map(SevenSegmentSearch.decipherDigits)
      val answer2 = outputValues.zip(deciphers).map(x => SevenSegmentSearch.getValue(x._1, x._2))

      assert(numberOfEasyDigits == 26)
      assert(answer2.sum == 61229L)
    }

    "work on single line input" in {
      val signalPatterns = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf".split(" ").take(10).toList
      val outputValues = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf".split(" ").drop(11).toList

      val decipher = SevenSegmentSearch.decipherDigits(signalPatterns)
      val answer = SevenSegmentSearch.getValue(outputValues, decipher)

      assert(answer == 5353L)
    }

    "give puzzle answer for part 1" in {
      val (_, outputValues) = SevenSegmentSearchTest.getInput("src/test/scala/day8/input.txt")
      val numberOfEasyDigits = outputValues.map(SevenSegmentSearch.getNumberOfEasyDigits).sum

      println(numberOfEasyDigits)
    }

    "give puzzle answer for part 2" in {
      val (signalPatterns, outputValues) = SevenSegmentSearchTest.getInput("src/test/scala/day8/input.txt")
      val deciphers = signalPatterns.map(SevenSegmentSearch.decipherDigits)
      val answer = outputValues.zip(deciphers).map(x => SevenSegmentSearch.getValue(x._1, x._2))

      println(answer.sum)
    }
  }
}

object SevenSegmentSearchTest {

  def getInput(filename: String): (List[List[String]], List[List[String]]) = {
    val input = general.ReadInTestData.getPuzzleInput(identity)(filename).map(_.split(" "))
    val outputValues = input.map(_.drop(11).toList)
    val signalPatterns = input.map(_.take(10).toList)
    (signalPatterns, outputValues)
  }

}
