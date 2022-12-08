package day8

object TreeHouse {

  type Coordinate = (Int, Int)
  type TreeMap = Map[Coordinate, Int]

  private def readAsMap(list: List[List[Int]]): TreeMap =
    list.map(_.zipWithIndex).zipWithIndex.flatMap { case (l, i) => l.map { case (v, j) => (i, j) -> v } }.toMap

  def outsideTrees(list: List[List[Int]]): Int =
    2 * (list.length + list.head.length - 2)

  private def visibleTrees(treeMap: TreeMap, coordinates: Coordinate, range: List[Int], horizontal: Boolean): List[Int] =
    if (horizontal) range.takeWhile(int => treeMap(coordinates) > treeMap((coordinates._1, int)))
    else range.takeWhile(int => treeMap(coordinates) > treeMap((int, coordinates._2)))

  private def isVisible(treeMap: TreeMap, coordinates: Coordinate, range: List[Int], horizontal: Boolean): Boolean =
    visibleTrees(treeMap, coordinates, range, horizontal) == range

  private def viewingDistance(treeMap: TreeMap, coordinates: Coordinate, range: List[Int], horizontal: Boolean): Int =
    if (isVisible(treeMap, coordinates, range, horizontal)) range.length
    else visibleTrees(treeMap, coordinates, range, horizontal).length + 1

  def solve[A](list: List[List[Int]])(f: (TreeMap, Coordinate, List[Int], Boolean) => A, g: (A, A) => A, h: List[A] => Int): Int = {
    val treeMap: TreeMap = readAsMap(list)
    val coords = Range(1, list.length - 1).flatMap(i => Range(1, list.head.length - 1).map(j => (i, j))).toList
    h(coords.map { coordinate =>
      List(
        f(treeMap, coordinate, Range(0, coordinate._2).reverse.toList, true),
        f(treeMap, coordinate, Range(coordinate._2 + 1, list.length).toList, true),
        f(treeMap, coordinate, Range(0, coordinate._1).reverse.toList, false),
        f(treeMap, coordinate, Range(coordinate._1 + 1, list.length).toList, false)
      ).reduce(g)
    })
  }

  def insideTrees(list: List[List[Int]]): Int =
    solve(list)(isVisible, _ || _, _.count(_ == true))

  def maxScore(list: List[List[Int]]): Int =
    solve(list)(viewingDistance, _ * _, _.max)

}
