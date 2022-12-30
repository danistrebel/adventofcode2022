import scala.collection.mutable
import scala.io.Source

case class Motion(direction: Char, distance: Int)
object day09 extends App {
  private val headMotions = Source.fromURI(getClass.getResource("day09.txt").toURI)
    .getLines().toList.map(s => {
    val splits = s.split(" ")
    Motion(splits(0).charAt(0), splits(1).toInt)
  })


  def simulateMotion(motion: Motion, snake: List[(Int, Int)]): List[(Int, Int)] = {
    val headPosition = snake(0)
    val nextHeadPosition = motion.direction match {
      case 'R' => (headPosition._1+1, headPosition._2)
      case 'L' => (headPosition._1-1, headPosition._2)
      case 'U' => (headPosition._1, headPosition._2-1)
      case 'D' => (headPosition._1, headPosition._2+1)
      case _ => throw new Exception(s"Unknown direction: ${motion.direction}")
    }

    var newSnake = nextHeadPosition::snake.tail

    for (i <- 0 to newSnake.length-2) {
      if (Math.abs(newSnake(i)._1 - newSnake(i+1)._1) <= 1 && Math.abs(newSnake(i)._2 - newSnake(i+1)._2) <= 1) {
        // No tail Movement Needed
      } else {
        val dX = newSnake(i)._1 - newSnake(i+1)._1
        val dY = newSnake(i)._2 - newSnake(i+1)._2

        if (dX == 0) { // move vertical
          newSnake = newSnake.take(i+1):::(newSnake(i+1)._1, newSnake(i+1)._2 + (dY / Math.abs(dY))) :: snake.drop(i+2)
        } else if (dY == 0) { // move horizontal
          newSnake = newSnake.take(i+1):::(newSnake(i+1)._1 + (dX / Math.abs(dX)), newSnake(i+1)._2) :: snake.drop(i+2)
        } else {
          newSnake = newSnake.take(i+1):::(newSnake(i+1)._1 + (dX / Math.abs(dX)), newSnake(i+1)._2 + (dY / Math.abs(dY))) :: snake.drop(i+2)
        }
      }
    }
    newSnake
  }

  var snake2 = (0 to 1).toList.map(_ => (0,0))

  private val tailSteps2 = mutable.HashSet[(Int, Int)]()

  for (motion <- headMotions) {
    for (_ <- 1 to motion.distance) {
      val newSnake = simulateMotion(motion, snake2)
      snake2 = newSnake
      tailSteps2.add(snake2(snake2.length-1))
    }
  }

  println(s"Day9 Part 1: ${tailSteps2.size}")

  var snake10 = (0 to 9).toList.map(_ => (0, 0))

  private val tailSteps10 = mutable.HashSet[(Int, Int)]()

  for (motion <- headMotions) {
    for (_ <- 1 to motion.distance) {
      val newSnake = simulateMotion(motion, snake10)
      snake10 = newSnake
      tailSteps10.add(snake10(snake10.length - 1))
    }
  }

  println(s"Day9 Part 2: ${tailSteps10.size}")
}
