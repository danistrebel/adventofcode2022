import scala.io.Source


object day02 extends App {

  /**
   * Returns Player 2's win points as 0 for lost, 3 for draw and 6 for win
   * @param player1
   * @param player2
   */
  private def winPoints(player1: Int, player2: Int): Int = {
    if (player1 == player2) return 3
    if (player1 == 1 && player2 == 3) return 0
    if (player1 == 2 && player2 == 1) return 0
    if (player1 == 3 && player2 == 2) return 0
    return 6
  }

  private def shapePoints(s: Char): Int = s match {
    case 'X' => 1 //R
    case 'Y' => 2 //P
    case 'Z' => 3 //S
    case 'A' => 1
    case 'B' => 2
    case 'C' => 3
  }

  def play(them: Char, me: Char): Int = {
    return shapePoints(me) + winPoints(shapePoints(them), shapePoints(me))
  }

  def win(them: Char): Char = {
    them match {
      case 'A' => 'Y'
      case 'B' => 'Z'
      case 'C' => 'X'
    }
  }

  def lose(them: Char): Char = {
    them match {
      case 'A' => 'Z'
      case 'B' => 'X'
      case 'C' => 'Y'
    }
  }

  def outcomePlay(them: Char, outcome: Char): Int = {
    outcome match {
      case 'X' => play(them, lose(them))
      case 'Y' => play(them, them)
      case 'Z' => play(them, win(them))
    }
  }

  private val totalScore = Source.fromURI(getClass.getResource("day02.txt").toURI).getLines().map(l => play(l.charAt(0), l.charAt(2))).sum
  println(s"Day2 Part1: ${totalScore}")

  private val totalScoreOutcomePlay = Source.fromURI(getClass.getResource("day02.txt").toURI).getLines().map(l => outcomePlay(l.charAt(0), l.charAt(2))).sum
  println(s"Day2 Part2: ${totalScoreOutcomePlay}")
}
