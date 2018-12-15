package tombrtls.adventofcode.day9

import tombrtls.adventofcode.Assignment


object MarbleManiaAssignment2 extends Assignment[(Int, Int), Long] {
  def main(args: Array[String]): Unit = startAssignment

  override val day: Int = 9
  override val testCases: Seq[(String, Long)] = Seq(
    ("sample_0.txt", 32),
    ("sample_1.txt", 8317),
    ("sample_2.txt", 146373),
    ("sample_3.txt", 2764),
    ("sample_4.txt", 54718),
    ("sample_5.txt", 37305)
  )
  override val inputFileName: String = "input.txt"

  val gameRegex = "(\\d*) players; last marble is worth (\\d*) points".r
  override def processLines(lines: Seq[String]): (Int, Int) = {
    lines.head match {
      case gameRegex(players, marble) => (players.toInt, marble.toInt * 100)
    }
  }

  override def implementation(input: (Int, Int)): Long = {
    val (players, marbles) = input

    val endState = (1 to marbles)
      .foldLeft(Game(players)) { (gameState, marble) => gameState.playMarble(marble) }

    endState
      .scores
      .max
  }
}
