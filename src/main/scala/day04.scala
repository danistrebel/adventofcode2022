import scala.io.Source

case class Assignment(start: Int, end: Int) {

  private def containedInSide(other: Assignment): Boolean = {
    if (start <= other.start && end >= other.end) {
      true
    } else
      false
  }

  def containedIn(other: Assignment): Boolean = {
    containedInSide(other) || other.containedInSide(this)
  }

  private def overlapsSide(other: Assignment): Boolean = {
    if (start <= other.end && end >= other.start) {
      true
    } else {
      false
    }
  }

  def overlaps(other: Assignment): Boolean = {
    overlapsSide(other) || other.overlapsSide(this)
  }
}

object day04 extends App {
  private val assignments = Source.fromURI(getClass.getResource("day04.txt").toURI)

  private val assignmentPairs = assignments.getLines().map(l => {
    l.split(",").map(_.split("-").map(_.toInt)).map(a => Assignment(a(0), a(1))).toList.take(2)
  }).toList

  private val containedCount = assignmentPairs.count(p => p(0).containedIn(p(1)))
  println(s"Day4 Part1 ${containedCount}")

  private val overlapsCount = assignmentPairs.count(p => p(0).overlaps(p(1)))
  println(s"Day4 Part2 ${overlapsCount}")
}
