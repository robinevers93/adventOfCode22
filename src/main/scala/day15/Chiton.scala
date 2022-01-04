package day15

import utils.Node
import utils.Node.GridMap

import scala.annotation.tailrec

class Chiton(edgeWeights: GridMap[Int]) {

  def getPathLength(currentNode: Node, adjacentNode: Node, distances: GridMap[Int]): Int =
    distances(currentNode) + edgeWeights(adjacentNode)

  def getNodesToUpdate(currentNode: Node, distances: GridMap[Int]): Set[Node] =
    currentNode.getAdjacents(edgeWeights).filter( adjacentNode =>
      !distances.contains(adjacentNode) || getPathLength(currentNode, adjacentNode, distances) < distances(adjacentNode))

  def getRemainingNodes(previousRemainingNodes: Set[Node], currentNode: Node, distances: GridMap[Int]): Set[Node] =
    previousRemainingNodes - currentNode ++ getNodesToUpdate(currentNode, distances)

  def updateDistances(currentNode: Node, distances: GridMap[Int]): GridMap[Int] =
    getNodesToUpdate(currentNode, distances).foldLeft(distances) { case (distances, node) =>
        distances.updated(node, getPathLength(currentNode, node, distances)) }

  def dijkstra(startNode: Node, endNode: Node): Int = {
    @tailrec
    def go(remaining: Set[Node], distances: GridMap[Int]): Int = remaining.minBy(distances) match {
      case currentNode: Node if currentNode == endNode => distances(endNode)
      case currentNode: Node =>
        val newRemaining = getRemainingNodes(remaining, currentNode, distances)
        val distance = updateDistances(currentNode, distances)
        go(newRemaining, distance)
    }
    go(Set(startNode), Map(startNode -> 0))
  }

}

object Chiton{

  def inputToGridMap(input: List[String]): GridMap[Int] = (for {
    x <- input.indices
    y <- 0 until input.head.length
  } yield Node(x,y) -> (input(x)(y).toInt - 48)).toMap

  def getFullMap(gridMap: GridMap[Int], gridSize: Int): GridMap[Int] = (for {
      x <- 0 until 5
      y <- 0 until 5
      (node, i) <- gridMap
    } yield Node(node.x + gridSize * x, node.y + gridSize * y) -> ((x + y + i - 1) % 9 + 1)).toMap

}
