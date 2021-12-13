package day10

import org.scalatest.wordspec.AnyWordSpec

class SyntaxScoringTest extends AnyWordSpec{

  "Sytax Scoring" should {
    "correctly identify corrupt lines" in {
      val corruptLine = "{([(<{}[<>[]}>{[]{[(<()>".toList
      val validLine = "[<>({}){}[([])<>]]".toList

      assert(SyntaxScoring.removeLegalPairsOnce(validLine) == "[()[()]]".toList)
      assert(SyntaxScoring.getSyntaxErrorScore(validLine) == 0)
      assert(SyntaxScoring.getSyntaxErrorScore(corruptLine) == 1197)
    }

    "find the syntax error score" in {
      val input = general.ReadInTestData.getPuzzleInput(_.toList)("src/test/scala/day10/sampleData.txt")
      assert(input.map(SyntaxScoring.getSyntaxErrorScore).sum == 26397)
    }

    "completing line successfully" in {
      val incompleteLine1 = "[({(<(())[]>[[{[]{<()<>>".toList
      val incompleteLine2 = "<{([{{}}[<[[[<>{}]]]>[]]".toList
      assert(SyntaxScoring.getAutocompleteScore(incompleteLine1) == 288957)
      assert(SyntaxScoring.getAutocompleteScore(incompleteLine2) == 294)
    }

    "get the middle score successfully" in {
      val input = general.ReadInTestData.getPuzzleInput(_.toList)("src/test/scala/day10/sampleData.txt")
      val scores = SyntaxScoring.getIncompleteLines(input).map(SyntaxScoring.getAutocompleteScore)
      assert(SyntaxScoring.getMiddleScore(scores) == 288957L)
    }
  }

}
