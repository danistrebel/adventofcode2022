import day01.elves

import scala.collection.mutable.ListBuffer
import scala.io.Source


object day01 extends App {
  private val elves = new ListBuffer[List[String]]

  private val elfCalories = new ListBuffer[String]
  for (l <- Source.fromURI(getClass.getResource("day1.txt").toURI).getLines()) {
    if (l.equals("")) {
      elves.append(elfCalories.toList)
      elfCalories.clear()
    } else {
      elfCalories.append(l)
    }
  }
  elves.append(elfCalories.toList)
  private val elvesTotalCalories = elves.map(e => e.map(c => c.toInt).sum)
  private val maxCalories = elvesTotalCalories.max
  println(s"Day1 Part 1: ${maxCalories} calories")

  private val elvesTopThree = elvesTotalCalories.sorted.takeRight(3).sum
  println(s"Day1 Part 2: ${elvesTopThree} calories")
}
