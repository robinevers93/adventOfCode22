package day12

object PassagePathing {

  type CaveMapping = Map[String, Set[String]]

  case class Connection(cave1: String, cave2: String){
    def connectedTo(cave: String): Option[String] = cave match {
      case c if c == cave1 => Some(cave2)
      case c if c == cave2 => Some(cave1)
      case _ => None
    }
  }

  def inputToCaveMapping(input: List[String]): CaveMapping = {
    val caves = input.flatMap(line => line.split('-')).toSet
    val connections: Set[Connection] = input.map(line => Connection(line.split('-').head, line.split('-')(1))).toSet
    caves.map(cave => cave -> connections.flatMap(_.connectedTo(cave))).toMap
  }

  def removeCavesFromMapping(caves: Set[String], mapping: CaveMapping): CaveMapping =
    mapping.removedAll(caves).view.mapValues(_ -- caves).toMap

  def isSmallCave(cave: String): Boolean = cave.charAt(0).isLower

  def getValidMoves(currentCave: String, mapping: CaveMapping, visitedCaves: Set[String], smallCaveVisitedTwice: Boolean): (Set[String], CaveMapping, Set[String], Boolean) = {
    val newVisited = if (isSmallCave(currentCave)) visitedCaves + currentCave else visitedCaves
    val newVisitedTwice = smallCaveVisitedTwice || (isSmallCave(currentCave) && visitedCaves.contains(currentCave))
    val newMapping = if (isSmallCave(currentCave) && newVisitedTwice) removeCavesFromMapping(newVisited, mapping) else mapping
    (mapping(currentCave), newMapping, newVisited, newVisitedTwice)
  }

  def findPaths(paths: Set[List[String]], mapping: CaveMapping, visitedCaves: Set[String], smallCaveVisitedTwice: Boolean): Set[List[String]] = paths.flatMap {
    case path if !mapping.keys.exists(_ == path.head)=> Set()
    case path if path.head == "end" => List(path)
    case path =>
      val (nextCaves, newMapping, newVisited, newVisitedTwice) = getValidMoves(path.head, mapping, visitedCaves, smallCaveVisitedTwice)
      findPaths(nextCaves.map(cave => path.prepended(cave)), newMapping, newVisited, newVisitedTwice)
  }

  def findPathsFromStart(mapping: CaveMapping, allowSmallCaveTwice: Boolean): Set[List[String]] =
    findPaths(mapping("start").map(List(_, "start")), removeCavesFromMapping(Set("start"), mapping), Set(), !allowSmallCaveTwice)

}
