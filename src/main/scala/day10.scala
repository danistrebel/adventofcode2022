import scala.io.Source

object day10 extends App {
  private val instructions = Source.fromURI(getClass.getResource("day10.txt").toURI)
    .getLines().toList

  def handleInterestingSignal(cycle: Int, register: Int): Int = {
    if ((cycle - 20) % 40 == 0) {
      println(cycle, register)
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
         cycle += 1
       }
       case s if s.startsWith("addx") => {
         sumInterestingSignals+=handleInterestingSignal(cycle, register)
         cycle+=1
         val opValue = s.split(" ")(1).toInt
         sumInterestingSignals+=handleInterestingSignal(cycle, register)
         register+=opValue
         cycle+=1
       }
     }
   })

  println(s"Day1 Part1: ${sumInterestingSignals}")
}
