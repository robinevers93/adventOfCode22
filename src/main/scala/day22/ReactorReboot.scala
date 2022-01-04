package day22

import utils.LazyCoordinates

import scala.annotation.tailrec

object ReactorReboot {

  case class Coordinate(x: Int, y: Int, z: Int)
  type Cuboid = Set[Coordinate]
  type LazyCuboid = Set[LazyCoordinates]
  case class Instruction(coordinates: LazyCoordinates, onOff: Boolean)

  @tailrec
  def part1(input: List[String], cuboid: Cuboid): Cuboid = input match {
    case s"$onOff x=$x1..$x2,y=$y1..$y2,z=$z1..$z2" :: t =>
      val coordinates = LazyCoordinates(x1.toInt, x2.toInt, y1.toInt, y2.toInt, z1.toInt, z2.toInt)
        .restrictRange(-50, 50)
        .toCoordinates
      part1(t, if (onOff == "on") cuboid ++ coordinates else cuboid -- coordinates)
    case _ => cuboid
  }

  @tailrec
  def inputToInstructions(input: List[String], acc: Vector[Instruction] = Vector()): Vector[Instruction] = input match {
    case s"$onOff x=$x1..$x2,y=$y1..$y2,z=$z1..$z2" :: t =>
      val coordinates = LazyCoordinates(x1.toInt, x2.toInt, y1.toInt, y2.toInt, z1.toInt, z2.toInt)
      inputToInstructions(t, acc.appended(Instruction(coordinates, onOff == "on")))
    case _ => acc
  }

  def removeIntersection(coordinates: LazyCoordinates, intersection: LazyCoordinates): LazyCuboid =
    coordinates.split(intersection).toSet - intersection

  def getIntersection(cuboid: LazyCuboid, coordinates: LazyCoordinates): Set[LazyCoordinates] =
    cuboid.flatMap(_.intersect(coordinates))

  @tailrec
  def part2(instructions: List[Instruction], cuboid: LazyCuboid = Set()): LazyCuboid = instructions match {
    case Instruction(c, true) :: t if getIntersection(cuboid, c).isEmpty => //when c and cuboid are disjoint, turn all lights in c on
      part2(t, cuboid + c)
    case Instruction(c, true) :: t => //when c and cuboid have overlap, create new instruction to turn light on for c\cuboid
      val intersection = getIntersection(cuboid, c).head
      val newInstructions: Set[Instruction] = removeIntersection(c, intersection).map(Instruction(_, true))
      part2(newInstructions.toList ++ t, cuboid)
    case Instruction(c, _) :: t if getIntersection(cuboid, c).isEmpty => //when c and cuboid are disjoint, all lights in c are already off
      part2(t, cuboid)
    case Instruction(c, _) :: t => //when c and cuboid have overlap, remove intersection of c and each lazy coordinate in cuboid
      val newCuboid = cuboid.flatMap{ coordinates =>
        coordinates.intersect(c) match {
          case None => Set(coordinates)
          case Some(intersection) => removeIntersection(coordinates, intersection)
        }
      }
      part2(t, newCuboid)
    case _ => cuboid
  }


}
