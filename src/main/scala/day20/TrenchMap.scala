package day20

import utils.Node
import utils.Node.GridMap

object TrenchMap {

  def getNodeValue[A](node: Node, grid: GridMap[A], algorithm: List[Char]): Char = {
    val binaryIndex = node.getAdjacentsInOrder
      .map(n => if (grid.contains(n)) grid(n) else grid(Node(0,0)))
      .map{ case '.' => 0; case '#' => 1}
      .mkString("")

    algorithm(Integer.parseInt(binaryIndex, 2))
  }

  def updateGridN(n: Int, gridMap: GridMap[Char], algorithm: List[Char]): GridMap[Char] = {
    def update(gridMap: GridMap[Char]): GridMap[Char] =
      Node.updateGridMap(gridMap.toList.map(_._1), gridMap, getNodeValue[Char](_,_,algorithm))

    Function.chain(List.fill(n)(update _))(gridMap)
  }

}
