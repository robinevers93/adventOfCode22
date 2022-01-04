package day17

import day17.TrickShot.trickShot
import org.scalatest.wordspec.AnyWordSpec

class TrickShotTest extends AnyWordSpec{

  "Trick shot" should {
    "give puzzle answers" in {

      val xmin = 70
      val xmax = 96
      val ymin = -179
      val ymax = -124

      assert(trickShot(xmin, xmax, ymin, ymax).max == 15931)

      assert(trickShot(xmin, xmax, ymin, ymax).size == 2555)


    }
  }

}
