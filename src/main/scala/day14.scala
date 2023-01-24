import scala.io.Source
import scala.util.control.Breaks._

object Grid {
  type Grid = Array[Array[CellType]]

  def print(grid: Grid): Unit = {
    for (elem <- grid) {
      println(elem.map(_.toString).mkString)
    }
  }
}

sealed trait CellType
case object Sand extends CellType {
  override def toString: String = "o"
}
case object Air extends CellType {
  override def toString: String = "."
}

case object Rock extends CellType {
  override def toString: String = "#"
}
object day14 extends App {

  private val rockEdges = Source.fromURI(getClass.getResource("day14.txt").toURI).getLines().toList
    .map(_.split(" -> ").map(_.split(",").map(_.toInt)).map(l => (l(0), l(1))))

  private val maxDepth = rockEdges.map(_.map(_._2).max).max+1
  private val minWidth = rockEdges.map(_.map(_._1).min).min-1
  private val maxWidth = rockEdges.map(_.map(_._1).max).max+2

  def normalizeWidth(original: Int, normalization:  Int): Int = { original - normalization}

  val grid: Array[Array[CellType]] = Array.fill(maxDepth+1) (Array.fill(maxWidth-minWidth)(Air))

  rockEdges.foreach(edges => edges.sliding(2).foreach(startEnd => {
    val start = (normalizeWidth(startEnd(0)._1, minWidth), startEnd(0)._2)
    val end = (normalizeWidth(startEnd(1)._1, minWidth), startEnd(1)._2)

    if (start._1 == end._1) {
      for (d <- start._2 to end._2 by (end._2-start._2)/Math.abs(end._2-start._2)) {
        grid(d)(start._1) = Rock
      }
    } else if (start._2 == end._2) {
      for (r <- start._1 to end._1 by (end._1-start._1)/Math.abs(end._1-start._1)) {
        grid(start._2)(r) = Rock
      }
    }
  }))

  var countSand = 0
  var reachedAbyss = false

  while(!reachedAbyss) {
    var sandPosition = (normalizeWidth(500, minWidth), 0)

    while (sandPosition._2<maxDepth && grid(sandPosition._2)(sandPosition._1) != Sand) {
      if (grid(sandPosition._2+1)(sandPosition._1) == Air) {
        sandPosition = (sandPosition._1, sandPosition._2+1)
      } else if (grid(sandPosition._2+1)(sandPosition._1-1) == Air) {
        sandPosition = (sandPosition._1-1, sandPosition._2+1)
      } else if (grid(sandPosition._2+1)(sandPosition._1+1) == Air) {
        sandPosition = (sandPosition._1+1, sandPosition._2+1)
      } else {
        grid(sandPosition._2)(sandPosition._1) = Sand
      }
    }

    countSand+=1

    if (sandPosition._2>=maxDepth) {
      reachedAbyss = true
      Grid.print(grid)
    }

  }

  println(s"Day14 Part1 ${countSand-1}")
}
