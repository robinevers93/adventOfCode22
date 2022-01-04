package day17

object TrickShot {

  case class Probe(x: Int, y: Int, dx: Int, dy: Int) {
    def move: Probe =
      Probe(x + dx, y + dy, if (dx > 0) dx - 1 else if (dx < 0) dx + 1 else 0, dy - 1)

    def isMoving(xmax: Int, ymin: Int): Boolean =
      x <= xmax && y>= ymin

    def isWithinTargetArea(xmin: Int, xmax: Int, ymin: Int, ymax: Int): Boolean =
      (x>= xmin) && (x <= xmax) && (y >= ymin) && (y <= ymax)
  }

  def listVelocities(xmax: Int, ymin: Int): Seq[(Int, Int)] = for {
    dx <- 0 to xmax;
    dy <- ymin to -ymin
  } yield (dx,dy)

  def getHighestPoint(dy: Int): Int =
    dy * (dy + 1) / 2 // 0 + dy + (dy-1) + (dy-2) + ... + 1 = 1 + 2 + 3 + .. + dy = dy * (dy + 1 ) / 2

  def trickShot(xmin: Int, xmax: Int, ymin: Int, ymax: Int): Seq[Int] = {
    def goesThroughTargetArea(dx: Int, dy: Int): Boolean = LazyList
      .iterate(Probe(0, 0, dx, dy))(_.move)
      .takeWhile(_.isMoving(xmax, ymin))
      .exists(_.isWithinTargetArea(xmin, xmax, ymin, ymax))

    listVelocities(xmax, ymin)
      .filter(v => goesThroughTargetArea(v._1, v._2))
      .map(v => getHighestPoint(v._2))
  }

}
