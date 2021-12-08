package day8

object SevenSegmentSearch {

  def getNumberOfEasyDigits(list: List[String]): Int =
    list.filterNot( x=> List(5,6).contains(x.length)).length

  def lengthToDigit(int: Int): Int = int match {
    case 2 => 1
    case 4 => 4
    case 3 => 7
    case 7 => 8
    case _ => 0
  }

  def getValue(list: List[String], decipher: Map[Int, Set[Char]]): Long =
    list.map(entry => decipher.map(_.swap)(entry.toSet))
      .map(_.toString).mkString("").toLong

  def decipherDigits(list: List[String]): Map[Int, Set[Char]] = {

    def lengthNEncodings(list: List[String], n: Int): List[Set[Char]] =
      list.map(_.toSet).filter(_.size == n)

    val map1478 = list
      .filterNot(x => List(5,6).contains(x.length))
      .map( x => lengthToDigit(x.length) -> x.toSet).toMap

    val map023569 = for {
      map3 <- lengthNEncodings(list, 5).find(x => map1478(1).subsetOf(x))
      map6 <- lengthNEncodings(list, 6).find(x => !map1478(1).subsetOf(x))
      map5 <- lengthNEncodings(list, 5).find(x => x.subsetOf(map6))
      map9 <- lengthNEncodings(list, 6).find(x => map1478(4).subsetOf(x))
      map0 <- lengthNEncodings(list, 6).find(x => !(x == map6 || x == map9))
      map2 <- lengthNEncodings(list, 5).find(x => !(x == map3 || x == map5))
    } yield Map(0 -> map0, 2 -> map2, 3 -> map3,5 -> map5, 6 -> map6, 9 -> map9)

    map023569 match {
      case None => throw new RuntimeException("Can't find solution")
      case Some(map) => map1478 ++ map
    }

  }
}
