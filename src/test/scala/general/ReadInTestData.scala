package general

import scala.io.{BufferedSource, Source}

object ReadInTestData {

  def getPuzzleInput(filename: String): String = {
    val bufferedSource = Source.fromFile(filename)
    val puzzleAsString = bufferedSource.mkString
    bufferedSource.close
    puzzleAsString
  }
  
  def getPuzzleInputAsList(filename: String)(splitter: String): List[String] =
    getPuzzleInput(filename).split(splitter).toList
  
  def getPuzzleInputAsFormattedList[T](transform: String => T)(filename: String)(splitter: String): List[T] =
    getPuzzleInputAsList(filename)(splitter).map(transform)
    
  def getListsPuzzleInputAsFormattedLists[T](transform: String => T)(filename: String)(splitter: String): List[List[T]] =
    getPuzzleInputAsList(filename)(splitter).map(_.linesIterator.map(transform(_)).toList)
}
