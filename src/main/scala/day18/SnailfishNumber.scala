package day18

trait SnailfishNumber{
  def add(sfn: SnailfishNumber): SnailfishNumber
  def addToLeftMost(x: Int): SnailfishNumber
  def addToRightMost(x: Int): SnailfishNumber
  def explodeN(n: Int): Option[(Option[Int], SnailfishNumber, Option[Int])]
  def explode: Option[SnailfishNumber] = this.explodeN(0).map(_._2)
  def split: Option[SnailfishNumber]
  def magnitude: Int

  def reduce: SnailfishNumber = this.explode match {
    case Some(sfn) => sfn.reduce
    case None => this.split.map(_.reduce).getOrElse(this)
  }

}
