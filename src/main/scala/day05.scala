import scala.None
import scala.collection.mutable.ListBuffer
import scala.io.Source

object day05 extends App {
  private val scenario = Source.fromURI(getClass.getResource("day05.txt").toURI)
    .getLines().toList

  private val instructionsStart = scenario.indexWhere(_.isEmpty)+1

  private val stacksCount = scenario(instructionsStart-2).split(" ").filter(_.nonEmpty).map(_.toInt).max
  private val stacks = ListBuffer.range(0,stacksCount).map(_ => ListBuffer[Char]())

  for (i <- instructionsStart-3 to 0 by -1) {
    val setup = scenario(i)
    for (s <- 0 to stacksCount-1 by 1) {
      val charIndex = s*4+1
      if (setup.length >= charIndex) {
        val char = setup.charAt(charIndex)
        if (char != ' ') {
          stacks(s).prepend(char)
        }
      }
    }
  }

  val stacksAssignmentOne = stacks.clone()
  val stacksAssignmentTwo = stacks.clone()

  val instrPattern = """move (\d+) from (\d+) to (\d+)""".r


  scenario.drop(instructionsStart).foreach(instr => {
    val matches = instrPattern.findFirstMatchIn(instr) match {
      case Some(m) => m
      case None => throw new Exception()
    }
    val moveCount = matches.group(1).toInt
    val fromStack = matches.group(2).toInt - 1
    val toStack = matches.group(3).toInt - 1

    val moving = stacksAssignmentOne(fromStack).take(moveCount)
    stacksAssignmentOne(fromStack) = stacksAssignmentOne(fromStack).drop(moveCount)
    stacksAssignmentOne(toStack) = moving.reverse++stacksAssignmentOne(toStack)
  })

  scenario.drop(instructionsStart).foreach(instr => {
    val matches = instrPattern.findFirstMatchIn(instr) match {
      case Some(m) => m
      case None => throw new Exception()
    }
    val moveCount = matches.group(1).toInt
    val fromStack = matches.group(2).toInt - 1
    val toStack = matches.group(3).toInt - 1

    val moving = stacksAssignmentTwo(fromStack).take(moveCount)
    stacksAssignmentTwo(fromStack) = stacksAssignmentTwo(fromStack).drop(moveCount)
    stacksAssignmentTwo(toStack) = moving ++ stacksAssignmentTwo(toStack)
  })


  private val heads = stacksAssignmentOne.map(_.head).mkString
  println(s"Day5 Part1 ${heads}")

  private val headsTwo = stacksAssignmentTwo.map(_.head).mkString
  println(s"Day5 Part2 ${headsTwo}")
}
