package day7

import day7.NoSpaceOnDevice.Directory

import scala.annotation.tailrec

object NoSpaceOnDevice {

  case class Directory(name: String, parentDirectory: Option[Directory], sizeOfFiles: Int)

  def directorySize(dir: Directory, allDirectories: List[Directory]): Int = {
    val subDirectories: List[Directory] = allDirectories.filter(_.parentDirectory.contains(dir))
    if (subDirectories.isEmpty)
      dir.sizeOfFiles
    else
      dir.sizeOfFiles + subDirectories.map(directorySize(_, allDirectories)).sum
  }

  @tailrec
  def readDirectories(input: List[String], parentDirs: List[Directory] = List.empty, acc: List[Directory] = List.empty): List[Directory] = input.headOption match {
    case None => acc
    case Some("$ cd ..") =>
      readDirectories(input.tail, parentDirs.tail, acc)
    case Some(instruction) if instruction.startsWith("$ cd") =>
      val (remainingList, sizeOfDir) = readDirectoryContents(input.drop(2))
      val dir = Directory(instruction.split(" ")(2), parentDirs.headOption, sizeOfDir)
      readDirectories(remainingList, dir :: parentDirs, dir :: acc)
    case error =>
      println(s"Something went wrong - [instruction: $error].")
      readDirectories(input.tail, parentDirs, acc)
  }

  @tailrec
  def readDirectoryContents(input: List[String], size: Int = 0): (List[String], Int) = input.headOption match {
    case Some(instruction) if instruction.startsWith("dir") =>
      readDirectoryContents(input.tail, size)
    case Some(instruction) if instruction.head.isDigit =>
      readDirectoryContents(input.tail, input.head.takeWhile(_.isDigit).toInt + size)
    case _ =>
      (input, size)
  }

}
