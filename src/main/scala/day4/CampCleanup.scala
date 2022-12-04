package day4

case class CampCleanup(input: String) {
  val ints: List[Int] = input.split(',').flatMap(_.split('-')).map(_.toInt).toList
  val assignmentsLeft: Set[Int] = Range.inclusive(ints.head, ints(1)).toSet
  val assignmentsRight: Set[Int] = Range.inclusive(ints(2), ints(3)).toSet

  val leftContained: Boolean = assignmentsLeft.union(assignmentsRight) == assignmentsRight
  val rightContained: Boolean = assignmentsLeft.union(assignmentsRight) == assignmentsLeft

  val overlapping: Boolean = assignmentsLeft.intersect(assignmentsRight).nonEmpty
}

object CampCleanup {

  def numberOfOverlappingAssignments(campCleanups: List[CampCleanup]): Int =
    campCleanups.count(c => c.overlapping)

  def numberOfContainedAssignments(campCleanups: List[CampCleanup]): Int =
    campCleanups.count(c => c.leftContained || c.rightContained)

}
