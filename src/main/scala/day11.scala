import scala.collection.mutable.ListBuffer
import scala.io.Source

object Monkey {
  def parse(monkeyLines: Seq[String]): Monkey = {
    val startingItems = monkeyLines(1).trim.split(" ").toList.drop(2).map(_.filter(_.isDigit).toLong)
    val operationParams = monkeyLines(2).trim.split(" ").takeRight(2)
    val operation = (operationParams(0), operationParams(1)) match {
      case ("*", "old") => (old: Long) => old * old
      case ("*", factor) => (old: Long) => factor.toInt * old
      case ("+", factor) => (old: Long) => factor.toInt + old
    }
    val testDivisor = monkeyLines(3).trim.split(" ").last.toInt
    val testTrueMonkey = monkeyLines(4).trim.split(" ").last.toInt
    val testFalseMonkey = monkeyLines(5).trim.split(" ").last.toInt
    val test = (input: Long) => if (input % testDivisor == 0) testTrueMonkey else testFalseMonkey
    Monkey(ListBuffer.from(startingItems), operation, test, testDivisor)
  }
}
case class Monkey(items: ListBuffer[Long], operation: Long => Long, test: Long => Int, testDivisor: Int, var operationsCount: Long = 0)
object day11 extends App {

  private val monkeys = Source.fromURI(getClass.getResource("day11.txt").toURI)
    .getLines().grouped(7).toList.map(monkeyLines => Monkey.parse(monkeyLines))

  for (_ <- 1 to 20) {
    for (monkeyIndex <- 0 to monkeys.length-1) {
      val monkey = monkeys(monkeyIndex)
      for (item <- monkey.items) {
        val newWorryLevel = monkey.operation(item) / 3
        val nextMonkey = monkey.test(newWorryLevel)
        monkeys(nextMonkey).items.append(newWorryLevel)
        monkey.operationsCount+=1
      }
      monkey.items.clear()
    }
  }

  val busiestMonkeys = monkeys.sortBy(_.operationsCount).reverse
  println(s"Day11 Part 1 ${busiestMonkeys(1).operationsCount*busiestMonkeys(0).operationsCount}")

  private val lessWorriedMonkeys = Source.fromURI(getClass.getResource("day11.txt").toURI)
    .getLines().grouped(7).toList.map(monkeyLines => Monkey.parse(monkeyLines))

  val worryNormalization = lessWorriedMonkeys.map(_.testDivisor).product

  for (_ <- 1 to 10000) {
    for (monkeyIndex <- 0 to lessWorriedMonkeys.length - 1) {
      val monkey = lessWorriedMonkeys(monkeyIndex)
      for (item <- monkey.items) {
        val newWorryLevel = monkey.operation(item) % worryNormalization
        val nextMonkey = monkey.test(newWorryLevel)
        lessWorriedMonkeys(nextMonkey).items.append(newWorryLevel)
        monkey.operationsCount += 1
      }
      monkey.items.clear()
    }
  }

  val longTermBusyMonkeys = lessWorriedMonkeys.sortBy(_.operationsCount).reverse
  println(s"Day11 Part 2 ${longTermBusyMonkeys(1).operationsCount * longTermBusyMonkeys(0).operationsCount}")

}


