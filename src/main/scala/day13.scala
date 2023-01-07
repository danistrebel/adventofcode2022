import sun.misc.Signal

import scala.collection.immutable.::
import scala.collection.mutable.ListBuffer
import scala.io.Source

sealed trait Signal extends Comparable[Signal] {
  def compareSignalLists(left: List[Signal], right: List[Signal]): Int = {
    (left, right) match {
      case (l :: lt, r :: rt) => {
        val headOrder = l.compareTo(r)
        if (headOrder == 0) {
          compareSignalLists(lt, rt)
        } else {
          headOrder
        }
      }
      case (Nil, Nil) => 0
      case (Nil, _) => -1
      case (_, Nil) => 1
    }
  }

  override def compareTo(o: Signal): Int = {
    (this, o) match {
      case (a: Number, b: Number) => a.i compare b.i
      case (a: SignalList, b: SignalList) => compareSignalLists(a.l.toList, b.l.toList)
      case (a: Number, b: SignalList) => compareSignalLists(a :: Nil, b.l.toList)
      case (a: SignalList, b: Number) => compareSignalLists(a.l.toList, b :: Nil)
    }
  }

}
case class Number(i: Int) extends Signal {
  override def toString: String = i.toString
}
case class SignalList(l: ListBuffer[Signal], parent: SignalList) extends Signal {
  override def toString: String = s"[${l.map(_.toString).mkString(",")}]"
}

object day13 extends App {

  def parseSignal(s: String): Signal = {
    parseSignal(s.tail.dropRight(1), SignalList(ListBuffer(), null))
  }
  def parseSignal(s: String, acc: SignalList):Signal = {
   if (s.isEmpty) {
     acc
   } else if (s.head == '[') {
     parseSignal(s.tail, SignalList(ListBuffer(), acc))
   } else if (s.head == ']') {
     acc.parent.l.append(acc)
     parseSignal(s.tail, acc.parent)
   } else if (s.head.isDigit) {
     val numberString = s.takeWhile(_.isDigit)
     acc.l.append(Number(numberString.toInt))
     parseSignal(s.drop(numberString.length), acc)
   } else { // commas
     parseSignal(s.tail, acc)
   }
  }


  var pairs = Source.fromURI(getClass.getResource("day13.txt").toURI)
    .getLines().grouped(3).map(_.take(2).map(s => parseSignal(s))).toList

  val sumOfOrderedPairIndices = pairs.zipWithIndex.map({case (signals, index) => if (signals(0).compareTo(signals(1)) == -1) index+1 else 0}).sum

  println(s"Day13 Part1: ${sumOfOrderedPairIndices}")

  val allSignals = pairs.flatten
    .appended(parseSignal("[[2]]"))
    .appended(parseSignal("[[6]]"))

  val allSorted = allSignals.sorted
  val dividerProduct = allSorted.zipWithIndex.map({case (signal, index) => if (signal.toString == "[[2]]" || signal.toString == "[[6]]") index+1 else 1}).product

  println(s"Day13 Part2: ${dividerProduct}")

}