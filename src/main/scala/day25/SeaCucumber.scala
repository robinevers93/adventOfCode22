package day25

import utils.Node
import utils.Node.{GridMap, updateGridMap}

import scala.annotation.tailrec

object SeaCucumber {

  def updateEast(gridMap: GridMap[Char]): GridMap[Char] = {
    val moving = gridMap.filter(x => x._2 == '>' && gridMap(x._1.getNodeToEast(gridMap)) == '.').keys.toList
    val newPositions = moving.map(_.getNodeToEast(gridMap))
    val step1 = updateGridMap(moving, gridMap, (_: Node, _: GridMap[Char]) => '.')
    updateGridMap(newPositions, step1, (_: Node, _: GridMap[Char]) => '>')
  }

  def updateSouth(gridMap: GridMap[Char]): GridMap[Char] = {
    val moving = gridMap.filter(x => x._2 == 'v' && gridMap(x._1.getNodeToSouth(gridMap)) == '.').keys.toList
    val newPositions = moving.map(_.getNodeToSouth(gridMap))
    val step1 = updateGridMap(moving, gridMap, (_: Node, _: GridMap[Char]) => '.')
    updateGridMap(newPositions, step1, (_: Node, _: GridMap[Char]) => 'v')
  }

  def moveSeaCucumbers(gridMap: GridMap[Char]): GridMap[Char] =
    updateSouth(updateEast(gridMap))

  @tailrec
  def moveUntilStuck(gridMap: GridMap[Char], i: Int = 1): (GridMap[Char], Int) = {
    if (moveSeaCucumbers(gridMap) == gridMap)
      (gridMap, i)
    else
      moveUntilStuck(moveSeaCucumbers(gridMap), i+1)
  }

}
