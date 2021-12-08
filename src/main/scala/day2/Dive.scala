package day2

import scala.annotation.tailrec

object Dive {

  def adjustPosition(instruction: String, p: (Int, Int, Int)): (Int, Int, Int) = instruction match {
    case s"forward $x" => (p._1 + x.toInt, p._2 + p._3 * x.toInt, p._3)
    case s"down $x" => (p._1, p._2, p._3 + x.toInt)
    case s"up $x" => (p._1, p._2, p._3 - x.toInt)
  }

  @tailrec
  def getPosition(instructions: List[String], position: (Int, Int, Int)): Int = instructions match {
    case Nil => position._1 * position._2
    case h :: t => getPosition(t, adjustPosition(h, position))
  }

}
