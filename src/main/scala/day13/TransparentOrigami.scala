package day13

import scala.annotation.tailrec

object TransparentOrigami {

  type FoldInstruction = Set[(Int, Int)] => Set[(Int, Int)]

  def parseInput(input: List[String]): Set[(Int, Int)] =
    input.map{ case s"$x,$y" => (y.toInt, x.toInt)}.toSet

  def foldAlongY(matrix: Set[(Int,Int)], y: Int): Set[(Int,Int)] =
    matrix.filter(_._1 < y) ++ matrix.filter(_._1 > y).map(x => (2*y-x._1, x._2))

  def foldAlongX(matrix: Set[(Int,Int)], x: Int): Set[(Int,Int)] =
    matrix.filter(_._2 < x) ++ matrix.filter(_._2 > x).map(z => (z._1, 2*x-z._2))

  def printableAnswer(matrix: Set[(Int, Int)]): List[List[Char]] = {
    val (xMax, yMax) = matrix.max
    val expandedMatrix = for {
      x <- 0 to xMax
      y <- 0 to yMax
    } yield if (matrix.contains((x,y))) '#' else '.'
    expandedMatrix.toList.grouped(yMax+1).toList
  }

  def getInstructions(instructions: List[String]): List[FoldInstruction] = instructions.map {
    case s"fold along x=$x" => foldAlongX(_, x.toInt)
    case s"fold along y=$y" => foldAlongY(_, y.toInt)
  }

  @tailrec
  def executeInstructions(matrix: Set[(Int, Int)], instructions: List[FoldInstruction]): Set[(Int, Int)] = instructions match {
    case h :: t => executeInstructions(h(matrix), t)
    case Nil => matrix
  }
}
