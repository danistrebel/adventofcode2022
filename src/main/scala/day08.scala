import scala.io.Source

case class Tree(height: Int, var visibleFromEdge: Boolean = false)
object TreeUtils {

  type Forest = List[List[Tree]]
  def printForest(forest: Forest): Unit = {
    forest.foreach(r => {
      println(r.map(t => {
        val visibleMarker = if (t.visibleFromEdge) "*" else " "
        t.height.toString + visibleMarker
      }).toString)
    })
  }

  def isVisibleInForest(forest: Forest, pos: (Int, Int), height: Int, direction: (Int, Int)): Boolean = {
    val r = pos._1 - direction._1
    val c = pos._2 - direction._2

    // Edge reached
    if (r == -1 || r >= forest.length || c == -1 || c >= forest(r).length) {
      return true
    }

    if (forest(r)(c).height < height) {
      isVisibleInForest(forest, (r,c), height, direction)
    } else {
      false
    }
  }

  def visibleTrees(forest: Forest, pos: (Int, Int), height: Int, direction: (Int, Int)): Int = {
    val r = pos._1 - direction._1
    val c = pos._2 - direction._2

    // Edge reached
    if (r == -1 || r >= forest.length || c == -1 || c >= forest(r).length) {
      return 0
    }

    if (forest(r)(c).height < height) {
      1+visibleTrees(forest, (r, c), height, direction)
    } else {
      1
    }
  }
}
object day08 extends App {
  private val forest = Source.fromURI(getClass.getResource("day08.txt").toURI)
    .getLines().toList.map(_.toList.map(ch => Tree(ch.toInt - 48)))

  for (r <- 0 to forest.length-1) {
    for (c <- 0 to forest(r).length -1) {
      forest(r)(c).visibleFromEdge = List((0,1), (0,-1), (1, 0), (-1,0)).map(dir => TreeUtils.isVisibleInForest(forest, (r,c), forest(r)(c).height, dir)).contains(true)
    }
  }

  var maxScenicValue= -1
  for (r <- 0 to forest.length - 1) {
    for (c <- 0 to forest(r).length - 1) {
      val visibleTrees = List((0, 1), (0, -1), (1, 0), (-1, 0)).map(dir => TreeUtils.visibleTrees(forest, (r, c), forest(r)(c).height, dir))
      maxScenicValue = Math.max(maxScenicValue, visibleTrees.fold(1)((agg, next) => agg*next))
    }
  }

//  TreeUtils.printForest(forest)

  println(s"Day8 Part1: ${forest.map(_.count(_.visibleFromEdge)).sum}")
  println(s"Day8 Part2: ${maxScenicValue}")

}
