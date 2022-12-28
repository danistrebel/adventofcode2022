import scala.io.Source

object day06 extends App {
  private val datastreams = Source.fromURI(getClass.getResource("day06.txt").toURI)
    .getLines().toList

  datastreams.foreach(l => {

    val marker = l.toList.sliding(4).map(_.toSet.size == 4).indexWhere(_ == true) + 4
    println(s"Day6 Part 1 ${marker}")

    val messages = l.toList.sliding(14).map(_.toSet.size == 14).indexWhere(_ == true) + 14
    println(s"Day6 Part 2 ${messages}")
  })
}
