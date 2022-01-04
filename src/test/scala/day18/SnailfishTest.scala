package day18

import day18.Snailfish.getLargestMagnitude
import org.scalatest.wordspec.AnyWordSpec

class SnailfishTest extends AnyWordSpec {


  "explode" should {
    "work as expected" in {
      val test2 = Snailfish.parseSnailfishNumber((((((9, 8), 1), 2), 3), 4))
      val test2a = Some(PairedNumber(PairedNumber(PairedNumber(PairedNumber(SimpleNumber(0), SimpleNumber(9)), SimpleNumber(2)), SimpleNumber(3)), SimpleNumber(4)))
      val test3 = Snailfish.parseSnailfishNumber((7, (6, (5, (4, (3, 2))))))
      val test3a = Some(PairedNumber(SimpleNumber(7), PairedNumber(SimpleNumber(6), PairedNumber(SimpleNumber(5), PairedNumber(SimpleNumber(7), SimpleNumber(0))))))
      val test4 = Snailfish.parseSnailfishNumber(((6, (5, (4, (3, 2)))), 1))
      val test4a = Some(PairedNumber(PairedNumber(SimpleNumber(6), PairedNumber(SimpleNumber(5), PairedNumber(SimpleNumber(7), SimpleNumber(0)))), SimpleNumber(3)))
      val test5 = Snailfish.parseSnailfishNumber(((3, (2, (1, (7, 3)))), (6, (5, (4, (3, 2))))))
      val test5a = Some(PairedNumber(PairedNumber(SimpleNumber(3),PairedNumber(SimpleNumber(2),PairedNumber(SimpleNumber(8),SimpleNumber(0)))),PairedNumber(SimpleNumber(9),PairedNumber(SimpleNumber(5),PairedNumber(SimpleNumber(4),PairedNumber(SimpleNumber(3),SimpleNumber(2)))))))
      val test6 = Snailfish.parseSnailfishNumber(((3, (2, (8, 0))), (9, (5, (4, (3, 2))))))
      val test6a = Some(PairedNumber(PairedNumber(SimpleNumber(3), PairedNumber(SimpleNumber(2), PairedNumber(SimpleNumber(8), SimpleNumber(0)))), PairedNumber(SimpleNumber(9), PairedNumber(SimpleNumber(5), PairedNumber(SimpleNumber(7), SimpleNumber(0))))))

      assert(test2.explode == test2a)
      assert(test3.explode == test3a)
      assert(test4.explode == test4a)
      assert(test5.explode == test5a)
      assert(test6.explode == test6a)
    }
  }

  "reduce" should {
    "work as expected" in {
      val test = Snailfish.parseSnailfishNumber((((((4,3),4),4),(7,((8,4),9))),(1,1)))
      val answer = test.explode.get.explode.get.split.get.split.get.explode.get
      assert(test.reduce == answer)
    }
  }

  "sum" should {
    "work as expected" in {
      val test = List(
        Snailfish.parseSnailfishNumber((1,1)),
        Snailfish.parseSnailfishNumber((2,2)),
        Snailfish.parseSnailfishNumber((3,3)),
        Snailfish.parseSnailfishNumber((4,4)),
      )

      val answer = Snailfish.parseSnailfishNumber(((((1,1),(2,2)),(3,3)),(4,4)))

      assert(test.reduce(_ add _) == answer)

      val test2 = List(
        Snailfish.parseSnailfishNumber((1,1)),
        Snailfish.parseSnailfishNumber((2,2)),
        Snailfish.parseSnailfishNumber((3,3)),
        Snailfish.parseSnailfishNumber((4,4)),
        Snailfish.parseSnailfishNumber((5,5)),
      )

      val answer2 = Snailfish.parseSnailfishNumber(((((3,0),(5,3)),(4,4)),(5,5)))

      assert(test2.reduce(_ add _) == answer2)

      val test3 = List(
        Snailfish.parseSnailfishNumber((1,1)),
        Snailfish.parseSnailfishNumber((2,2)),
        Snailfish.parseSnailfishNumber((3,3)),
        Snailfish.parseSnailfishNumber((4,4)),
        Snailfish.parseSnailfishNumber((5,5)),
        Snailfish.parseSnailfishNumber((6,6)),
      )

      val answer3 = Snailfish.parseSnailfishNumber(((((5,0),(7,4)),(5,5)),(6,6)))

      assert(test3.reduce(_ add _) == answer3)
    }

    "work as expected for larger examples" in {
      val test = List(
        Snailfish.parseSnailfishNumber((((0,(4,5)),(0,0)),(((4,5),(2,6)),(9,5)))),
        Snailfish.parseSnailfishNumber((7,(((3,7),(4,3)),((6,3),(8,8))))),
        Snailfish.parseSnailfishNumber(((2,((0,8),(3,4))),(((6,7),1),(7,(1,6))))),
        Snailfish.parseSnailfishNumber(((((2,4),7),(6,(0,5))),(((6,8),(2,8)),((2,1),(4,5))))),
        Snailfish.parseSnailfishNumber((7,(5,((3,8),(1,4))))),
        Snailfish.parseSnailfishNumber(((2,(2,2)),(8,(8,1)))),
        Snailfish.parseSnailfishNumber((2,9)),
        Snailfish.parseSnailfishNumber((1,(((9,3),9),((9,0),(0,7))))),
        Snailfish.parseSnailfishNumber((((5,(7,4)),7),1)),
        Snailfish.parseSnailfishNumber(((((4,2),2),6),(8,7)))
      )

      val answer = Snailfish.parseSnailfishNumber(((((8,7),(7,7)),((8,6),(7,7))),(((0,7),(6,6)),(8,7))))
      assert(test.reduce(_ add _) == answer)

    }
  }

  "magnitude" should {

    "calculate magnitude successfully" in {
      assert(Snailfish.parseSnailfishNumber((9,1)).magnitude == 29)
      assert(Snailfish.parseSnailfishNumber(((9,1),(1,9))).magnitude == 129)
    }

    "get puzzle answer for part 1" in {

      val answer = Input.input.reduce(_ add _).magnitude
      assert(answer == 3806)

    }

    "solve part 2" in {

      val answer = getLargestMagnitude(Input.input)
      assert(answer == 4727)
    }
  }
}
