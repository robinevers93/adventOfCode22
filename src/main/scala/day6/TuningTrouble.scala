package day6

import scala.annotation.tailrec

object TuningTrouble {

  @tailrec
  def solve(list: List[Char], size: Int, acc: Vector[List[Char]] = Vector.empty): Option[Int] = {
    if (list.size < size)
      None
    else if (list.take(size).distinct.size == size)
      Some(acc.size + size)
    else
      solve(list.tail, size, acc :+ list.take(size))
  }

}
