import scala.io.Source

object day03 extends App {

  def priority(c: Char): Int = {
    if (c >= 'a' && c <='z')
      c - 'a' + 1
    else
      c - 'A' + 27
  }
  private val chars = Source.fromURI(getClass.getResource("day03.txt").toURI).getLines().map(rucksack => {
    val splits = rucksack.splitAt(rucksack.length/2)
    splits._1.toSet.intersect(splits._2.toSet).toList.head
  })
  private val values = chars.map(priority)
  println(s"Day3 Part1 ${values.sum}")

  private val groupItems = Source.fromURI(getClass.getResource("day03.txt").toURI)
    .getLines()
    .grouped(3).map(group => group.tail.foldLeft(group.head) { (agg: String, next: String) => agg.intersect(next) })
    .map(_.toList.head)

  private val groupValues = groupItems.map(priority)

  println(s"Day3 Part2 ${groupValues.sum}")

}
