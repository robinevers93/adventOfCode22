package utils

import utils.Node.GridMap

case class Node(x: Int, y: Int){

  def getAdjacents(gridMap: GridMap[Int]): Set[Node] =
    Set(Node(x - 1, y), Node(x + 1, y), Node(x, y - 1), Node(x, y + 1)).filter(gridMap.contains)

  def getAdjacentsInOrder: List[Node] =
    List(
      Node(x - 1, y-1), Node(x - 1, y), Node(x - 1, y+1),
      Node(x, y-1), Node(x,y), Node(x,y+1),
      Node(x+1, y-1), Node(x+1, y), Node(x+1, y+1)
    )

  def getNodeToEast[A](gridMap: GridMap[A]): Node =
    if (gridMap.contains(Node(x,y+1))) Node(x,y+1) else Node(x,0)

  def getNodeToSouth[A](gridMap: GridMap[A]): Node =
    if (gridMap.contains(Node(x+1,y))) Node(x+1,y) else Node(0,y)
}

object Node {

  type GridMap[A] = Map[Node, A]

  def matrixToGridMap(input: List[String]): GridMap[Char] = (for {
    x <- input.indices
    y <- input.head.indices
  } yield Node(x,y) -> input(x)(y)).toMap

  def updateGridMap[A](nodes: List[Node], gridMap: GridMap[A], getNewValue: (Node, GridMap[A]) => A): GridMap[A] =
    nodes.foldLeft(gridMap) { case (grid, node) => grid.updated(node, getNewValue(node, gridMap)) }

  def fillMap[A](gridMap: GridMap[A], gridSize: Int, expantionFactor: Int, fill: A): GridMap[A] = (for {
    x <- 0 until expantionFactor
    y <- 0 until expantionFactor
    (node, i) <- gridMap
    newNode = Node(node.x + gridSize * x, node.y + gridSize * y)
  } yield newNode -> (if (x == expantionFactor/2 && y == expantionFactor/2) i else fill)).toMap


  def gridToMatrix[A](gridMap: GridMap[A], size: Int): List[List[A]] = (for {
      x <- 0 until size
      y <- 0 until size
    } yield Option(gridMap(Node(x,y))).getOrElse(gridMap(Node(0,0)))).toList.grouped(size).toList

}
