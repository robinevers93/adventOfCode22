package day18

case class SimpleNumber(x: Int) extends SnailfishNumber{
  def add(sfn: SnailfishNumber): SnailfishNumber =
    PairedNumber(SimpleNumber(x), sfn).reduce
  def addToLeftMost(z: Int): SimpleNumber =
    SimpleNumber(x + z)
  def addToRightMost(z: Int): SimpleNumber =
    SimpleNumber(x + z)
  def explodeN(n: Int): Option[(Option[Int], SnailfishNumber, Option[Int])] =
    None
  def split: Option[SnailfishNumber] =
    if (x >= 10) Some(PairedNumber(SimpleNumber((x/2.0).floor.toInt), SimpleNumber((x/2.0).ceil.toInt)))
    else None
  def magnitude: Int =
    x
}
