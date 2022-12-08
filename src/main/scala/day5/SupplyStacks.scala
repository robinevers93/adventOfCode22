package day5

import scala.annotation.tailrec
import scala.collection.immutable.List
import scala.util.Try

object SupplyStacks {

  case class Instruction(number: Int, from: Int, to: Int)

  def readInstructions(input: List[String]): List[Instruction] =
    input
      .flatMap(_.split("move|from|to"))
      .map(_.trim)
      .flatMap(_.toIntOption)
      .grouped(3)
      .map(instruction => Instruction(instruction.head, instruction(1) - 1, instruction(2) - 1))
      .toList

  def readRows(input: String): List[Option[Char]] =
    Range(0, 9).toList.map(i => Try(input(i * 4 + 1)).toOption).map{ case Some(' ') => None; case c => c }

  def readInitialState(input: List[String]): List[List[Char]] = {
    val rows = input.map(readRows)
    Range(0, 9).toList.map { column =>
      Range(0, input.length).toList.flatMap { row =>
        rows(row)(column)
      }
    }.filter(_.nonEmpty)
  }

  def executeInstruction(in: List[List[Char]], instruction: Instruction, reverse: Boolean): List[List[Char]] = {
    Range(0, in.length).toList.map { row =>
      if (row == instruction.to && reverse)
        in(instruction.from).take(instruction.number).reverse ++ in(row)
      else if (row == instruction.to)
        in(instruction.from).take(instruction.number) ++ in(row)
      else if (row == instruction.from)
        in(row).drop(instruction.number)
      else
        in(row)
    }
  }

  def executeInstructions(in: List[List[Char]], instructions: List[Instruction], reverse: Boolean): List[List[Char]] =
    instructions.foldLeft(in)((acc, instruction) => executeInstruction(acc, instruction, reverse))

}
