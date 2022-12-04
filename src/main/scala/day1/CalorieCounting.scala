package day1

object CalorieCounting {

  def maxCalories(list: List[List[Int]]): Int =
    list.map(_.sum).max

  def totalCaloriesMax3(list: List[List[Int]]): Int =
    list.map(_.sum).sorted.reverse.take(3).sum

}
