package tombrtls.adventofcode.day9

import tombrtls.adventofcode.Assignment

object MarbleManiaAssignment1 extends Assignment[Game, Int] {
  def main(args: Array[String]): Unit = startAssignment

  override val day: Int = 9
  override val testCases: Seq[(String, Int)] = Seq(
    ("sample_0.txt", 32),
    ("sample_1.txt", 8317),
    ("sample_2.txt", 146373),
    ("sample_3.txt", 2764),
    ("sample_4.txt", 54718),
    ("sample_5.txt", 37305)
  )
  override val inputFileName: String = "input.txt"

  val gameRegex = "(\\d*) players; last marble is worth (\\d*) points".r
  override def processLines(lines: Seq[String]): Game = {
    lines.head match {
      case gameRegex(players, marble) => Game(players.toInt, marble.toInt)
    }
  }

  override def implementation(input: Game): Int = {
    val game = (1 to input.maxMarble)
      .foldLeft(input) { (game, marble) => game.playMarble(marble) }

    game.scores
        .max
  }
}
