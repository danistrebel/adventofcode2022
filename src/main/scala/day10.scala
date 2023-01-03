import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object crt {
  private val pixels: Array[Boolean] = new Array(40*6)
  def setPixel(cycle: Int, sprite: Int): Unit = {
    val p = (cycle-1)
    val col = p%40
    if (Math.abs(sprite - col) <= 1) {
      pixels(p)=true
    } else {
      pixels(p)=false
    }
  }

  override def toString: String = {
    pixels.grouped(40).toArray.map(row => row.map(p => if (p) "#" else '.' ).mkString + "\n").mkString
  }
}

object day10 extends App {
  private val instructions = Source.fromURI(getClass.getResource("day10.txt").toURI)
    .getLines().toList

  def handleInterestingSignal(cycle: Int, register: Int): Int = {
    if ((cycle - 20) % 40 == 0) {
      cycle*register
    } else {
      0
    }
  }

  var sumInterestingSignals = 0
  var cycle = 1
  var register = 1

   instructions.foreach(instr => {
     instr match {
       case "noop" => {
         sumInterestingSignals += handleInterestingSignal(cycle, register)
         crt.setPixel(cycle, register)
         cycle += 1
       }
       case s if s.startsWith("addx") => {
         sumInterestingSignals+=handleInterestingSignal(cycle, register)
         crt.setPixel(cycle, register)
         cycle+=1
         val opValue = s.split(" ")(1).toInt
         sumInterestingSignals+=handleInterestingSignal(cycle, register)
         crt.setPixel(cycle, register)
         register+=opValue
         cycle+=1
       }
     }
   })

  println(s"Day1 Part1: ${sumInterestingSignals}")

  println("Day10 Part2")
  print(crt)
}
