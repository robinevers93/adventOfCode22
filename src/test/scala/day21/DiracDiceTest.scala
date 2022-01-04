package day21

import day21.DiracDice.{part1, part2}
import org.scalatest.wordspec.AnyWordSpec

class DiracDiceTest extends AnyWordSpec{

  "DiracDice" should {
    "solve part 1" in {
      val answer = part1(10, 9)
      assert(answer == 918081)
    }

    "solve part 2" in {
      val answer = part2(10,9)
      assert(answer == 158631174219251L)
    }
  }

}
