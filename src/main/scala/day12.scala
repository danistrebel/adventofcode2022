import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.control.Breaks.break

object day12 extends App{

  def printDistanceMatrix(matrix: Array[Array[Int]]): Unit = {
    println()
    matrix.foreach(row => {
      row.map(i => if (i == Int.MaxValue) "??? " else "%03d ".format(i)).foreach(print)
      println()
    })
  }

  val grid = Source.fromURI(getClass.getResource("day12.txt").toURI).getLines().map(_.toArray).toArray

  var start: (Int, Int) = null
  var end: (Int, Int) = null

  for (row <- 0 to grid.length-1) {
    for (col <- 0 to grid(row).length-1) {
      grid(row)(col) match {
        case 'S' => start = (row, col)
        case 'E' => end = (row, col)
        case _ =>
      }
    }
  }

  val distance = (1 to grid.length).map(_ => Array.fill(grid(0).length){ Int.MaxValue }).toArray

  distance(start._1)(start._2) = 0

  val fringe = new mutable.Queue[(Int, Int)]
  fringe.enqueue(start)

  while (! fringe.isEmpty) {
    val (x,y) = fringe.dequeue()
    List((-1, 0), (1, 0), (0,-1), (0,1)).foreach(delta => {
      val (neighborX, neighborY) = (x+delta._1, y+delta._2)
      if (neighborX < 0 || neighborY < 0 || neighborX >= grid.length || neighborY >= grid(0).length) {
        // skip
      } else {
        val neighborHeight = grid(neighborX)(neighborY) match {
          case 'E' => 'z'
          case c: Char => c
        }
        if (grid(x)(y) == 'S' || neighborHeight - grid(x)(y) <= 1) {
          val newDistanceFromStart = distance(x)(y)+1
          if (distance(neighborX)(neighborY) > newDistanceFromStart) {
            distance(neighborX)(neighborY) = distance(x)(y) + 1
            if (grid(neighborX)(neighborY) != 'E') {
              fringe.append((neighborX, neighborY))
            }
          }
        }
      }
    })
  }

  val distanceStoE = distance(end._1)(end._2)
  println(s"Day12 Part1: ${distanceStoE}")

  val distanceFromEnd = (1 to grid.length).map(_ => Array.fill(grid(0).length){Int.MaxValue }).toArray
  distanceFromEnd(end._1)(end._2)=0

  val fringeEnd = new mutable.Queue[(Int, Int)]
  fringeEnd.enqueue(end)

  var closestA: Option[Int] = None

  while (!fringeEnd.isEmpty) {
    val (x, y) = fringeEnd.dequeue()
    List((-1, 0), (1, 0), (0, -1), (0, 1)).foreach(delta => {
      val (neighborX, neighborY) = (x + delta._1, y + delta._2)
      if (neighborX < 0 || neighborY < 0 || neighborX >= grid.length || neighborY >= grid(0).length) {
        // skip
      } else {
        val neighborHeight = grid(neighborX)(neighborY) match {
          case 'S' => 'a'
          case c: Char => c
        }
        if (grid(x)(y) == 'E' || grid(x)(y)- neighborHeight <= 1) {
          val newDistanceFromStart = distanceFromEnd(x)(y) + 1
          if (distanceFromEnd(neighborX)(neighborY) > newDistanceFromStart) {
            distanceFromEnd(neighborX)(neighborY) = distanceFromEnd(x)(y) + 1
            fringeEnd.append((neighborX, neighborY))
          }
          if (neighborHeight == 'a') {
            closestA = Some(distanceFromEnd(neighborX)(neighborY))
            fringeEnd.clear()
          }
        }

      }
    })
  }

  println(s"Day12 Part2: ${closestA.get}")
}