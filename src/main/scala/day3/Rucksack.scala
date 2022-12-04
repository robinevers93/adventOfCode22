package day3

case class Rucksack(contents: List[Char]) {
  val (left: List[Char], right: List[Char]) = contents.splitAt(contents.size / 2)
  val duplicate: Char = left.find(right.contains(_))
    .getOrElse(throw new IllegalArgumentException("Invalid Rucksack - No duplicate found"))

  def findDuplicates(chars: List[Char]): List[Char] = this.contents.intersect(chars).distinct
}

object Rucksack {

  def apply(s: String): Rucksack = Rucksack(s.toList)

  def charToPriority(char: Char): Int = if (char.isLower) char.toInt - 96 else char.toInt - 38

  val allChars: List[Char] = ('A' to 'z').toList

  def sumOfDuplicatePriorities(rucksacks: List[Rucksack]): Int = rucksacks.map(r => charToPriority(r.duplicate)).sum

  def getBadges(rucksacks: List[Rucksack]): List[Char] = rucksacks.foldLeft(allChars)((acc, x) => x.findDuplicates(acc))

  def sumBadges(rucksacks: List[Rucksack]): Int = rucksacks.grouped(3).flatMap(getBadges).map(charToPriority).sum

}
