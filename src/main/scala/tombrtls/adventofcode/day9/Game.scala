package tombrtls.adventofcode.day9

case class Game(numberOfPlayers: Int, maxMarble: Int, circle: Circle, scores: Seq[Int]) {
  def playMarble(marble: Int): Game = {
    marble match {
      case special if marble != 0 && marble % 23 == 0 => {
        val playerIndex = (marble - 1) % scores.length
        val currentScore = scores(playerIndex)
        val (value, newCircle) = circle.remove7nthMarbleFromActive

        this.copy(
          scores = scores.updated(playerIndex, currentScore + marble + value),
          circle = newCircle
        )
      }
      case marble => {
        this.copy(
          circle = circle.placeMarble(marble)
        )
      }
    }
  }
}

object Game {
  def apply(numberOfPlayers: Int, maxMarble: Int): Game = {
    val scores = List.fill(numberOfPlayers)(0)
    Game(numberOfPlayers, maxMarble, Circle(List(0), 0), scores)
  }
}