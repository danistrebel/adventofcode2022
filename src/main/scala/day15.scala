import scala.collection.mutable
import scala.io.Source

case class Coordinate(x: Int, y: Int)
case class PuzzleInput(sensor: Coordinate, beacon: Coordinate, manhattanDistance: Int)

object Distance {
  def manhattanDistance(a: Coordinate, b: Coordinate): Int = Math.abs(a.x-b.x)+Math.abs(a.y-b.y)
}
object day15 extends App {

  val inputPattern = """Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)""".r
  val puzzleInputs = Source.fromURI(getClass.getResource("day15.txt").toURI).getLines().toList
    .map(line => line match {
    case inputPattern(sx, sy, bx, by) => {
      val sensor = Coordinate(sx.toInt, sy.toInt)
      val beacon = Coordinate(bx.toInt, by.toInt)
      PuzzleInput(sensor, beacon, Distance.manhattanDistance(sensor, beacon))
    }
  })

  // given in example
  val targetY = 2_000_000

  var negativeX = mutable.HashSet[Int]()
  var occupiedX = mutable.HashSet[Int]() // Beacons or Sensors on the target row

  for (puzzleInput <- puzzleInputs) {
    val targetUnderSensor = Coordinate(puzzleInput.sensor.x, targetY)

    if(puzzleInput.sensor.y == targetY) {
      occupiedX.add(puzzleInput.sensor.x)
    }
      if (puzzleInput.beacon.y == targetY) {
      occupiedX.add(puzzleInput.beacon.x)
    }

    val closerThanBeacon = puzzleInput.manhattanDistance - Distance.manhattanDistance(puzzleInput.sensor, targetUnderSensor)
    if (closerThanBeacon >= 0) {
      negativeX.add(targetUnderSensor.x)
      for (i <- 1 to closerThanBeacon) {
        negativeX.add(targetUnderSensor.x - i)
        negativeX.add(targetUnderSensor.x + i)
      }
    }
  }

  val totalNegativeX = negativeX.diff(occupiedX)

  println(s"Day15 Part1 ${totalNegativeX.size}")
}
