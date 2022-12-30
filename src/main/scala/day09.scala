import scala.collection.mutable
import scala.io.Source

case class Motion(direction: Char, distance: Int)
object day09 extends App {
  private val headMotions = Source.fromURI(getClass.getResource("day09.txt").toURI)
    .getLines().toList.map(s => {
    val splits = s.split(" ")
    Motion(splits(0).charAt(0), splits(1).toInt)
  })


  def simulateMotion(motion: Motion, headPosition: (Int, Int), tailPosition: (Int, Int)): ((Int, Int), (Int, Int)) = {

    val nextHeadPosition = motion.direction match {
      case 'R' => (headPosition._1+1, headPosition._2)
      case 'L' => (headPosition._1-1, headPosition._2)
      case 'U' => (headPosition._1, headPosition._2-1)
      case 'D' => (headPosition._1, headPosition._2+1)
      case _ => throw new Exception(s"Unknown direction: ${motion.direction}")
    }

    if (Math.abs(nextHeadPosition._1-tailPosition._1) <= 1 && Math.abs(nextHeadPosition._2-tailPosition._2) <= 1) {
      // No tail Movment Needed
      (nextHeadPosition, tailPosition)
    } else {
      val dX = nextHeadPosition._1 - tailPosition._1
      val dY = nextHeadPosition._2 - tailPosition._2

      if (dX == 0) { // move vertical
        (nextHeadPosition, (tailPosition._1, tailPosition._2+(dY/Math.abs(dY))))
      } else if (dY == 0) { // move horizontal
        (nextHeadPosition, (tailPosition._1 + (dX/ Math.abs(dX)), tailPosition._2))
      } else {
        (nextHeadPosition, (tailPosition._1 + (dX/ Math.abs(dX)), tailPosition._2+(dY/Math.abs(dY))))
      }
    }
  }

  var headPosition = (0, 0) //x,y
  var tailPosition = (0, 0) //x,y

  private val tailSteps = mutable.HashSet[(Int, Int)]()

  for (motion <- headMotions) {
    for (_ <- 1 to motion.distance) {
      val (nextHead, nextTail) = simulateMotion(motion, headPosition, tailPosition)
      headPosition = nextHead
      tailPosition = nextTail
      tailSteps.add(tailPosition)
    }
  }

  println(tailSteps.size)
}
