package day18

case class PairedNumber(x: SnailfishNumber, y: SnailfishNumber) extends SnailfishNumber{
  def add(sfn: SnailfishNumber): SnailfishNumber =
    PairedNumber(PairedNumber(x,y), sfn).reduce
  def addToLeftMost(z: Int): SnailfishNumber =
    PairedNumber(x.addToLeftMost(z), y)
  def addToRightMost(z: Int): SnailfishNumber =
    PairedNumber(x, y.addToRightMost(z))

  def explodeN(n: Int): Option[(Option[Int], SnailfishNumber, Option[Int])] = this match {
    case PairedNumber(SimpleNumber(left), SimpleNumber(right)) if n >= 4 =>
      Some((Some(left), SimpleNumber(0), Some(right)))
    case PairedNumber(left, right) if left.explodeN(n + 1).isDefined =>
      left.explodeN(n + 1).map { case (leftAdd, left, rightAdd) =>
        (leftAdd, PairedNumber(left, rightAdd.map(right.addToLeftMost).getOrElse(right)), None)}
    case PairedNumber(left, right) =>
      right.explodeN(n + 1).map { case (leftAdd, right, rightAdd) =>
        (None, PairedNumber(leftAdd.map(left.addToRightMost).getOrElse(left), right), rightAdd)}
  }

  def split: Option[SnailfishNumber] = (x.split, y.split) match {
    case (Some(sfn), _) => Some(PairedNumber(sfn, y))
    case (None, Some(sfn)) => Some(PairedNumber(x, sfn))
    case _ => None
  }

  def magnitude: Int =
    3 * x.magnitude + 2 * y.magnitude

}
