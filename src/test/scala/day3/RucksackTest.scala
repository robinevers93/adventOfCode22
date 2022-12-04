package day3

import org.scalatest.wordspec.AnyWordSpec

class RucksackTest extends AnyWordSpec {

  "the priority for characters" should {
    "be 1 to 52" in {
      assert(Rucksack.charToPriority('a') == 1)
      assert(Rucksack.charToPriority('p') == 16)
      assert(Rucksack.charToPriority('z') == 26)
      assert(Rucksack.charToPriority('A') == 27)
      assert(Rucksack.charToPriority('L') == 38)
      assert(Rucksack.charToPriority('Z') == 52)
    }
  }

  "splitting a rucksack in half" should {
    "result in two equally sized halfs" in {
      val rucksack = Rucksack("abccef")
      assert(rucksack.left == List('a','b','c'))
      assert(rucksack.right == List('c','e','f'))
    }
  }

  "finding duplicates" should {
    "identify the duplicate" in {
      val rucksack = Rucksack("vJrwpWtwJgWrhcsFMMfFFhFp")
      assert(rucksack.duplicate == 'p')

      val rucksack2 = Rucksack("jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL")
      assert(rucksack2.duplicate == 'L')
    }
  }

  "solving the first puzzle" should {
    "give outcome 157" in {
        val input = general.ReadInTestData.getPuzzleInputAsFormattedList(Rucksack.apply)("src/test/scala/day3/sampleData.txt")("\n")
        val answer = Rucksack.sumOfDuplicatePriorities(input)
        assert(answer == 157)
    }
    "print the answer" in {
      val input = general.ReadInTestData.getPuzzleInputAsFormattedList(Rucksack.apply)("src/test/scala/day3/input.txt")("\n")
      val answer = Rucksack.sumOfDuplicatePriorities(input)
      println(answer)
    }
  }

  "getBadges" should {
    "find chars that are in all rucksacks" in {
      val input = List(Rucksack("abca"), Rucksack("abda"), Rucksack("abea"))
      val answer = Rucksack.getBadges(input)
      assert(answer == List('a', 'b'))
    }
  }

  "the sum of badges" should {
    "be 70" in {
      val input = general.ReadInTestData.getPuzzleInputAsFormattedList(Rucksack.apply)("src/test/scala/day3/sampleData.txt")("\n")
      val answer = Rucksack.sumBadges(input)
      assert(answer == 70)
    }

    "print the answer to the second puzzle" in {
      val input = general.ReadInTestData.getPuzzleInputAsFormattedList(Rucksack.apply)("src/test/scala/day3/input.txt")("\n")
      val answer = Rucksack.sumBadges(input)
      println(answer)
    }
  }


}
