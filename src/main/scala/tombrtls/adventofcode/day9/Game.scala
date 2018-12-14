package tombrtls.adventofcode.day9

case class Game(circle: Circle[Int], scores: List[Long]) {
  def playMarble(marble: Int): Game = {
    marble match {
      case special if marble % 23 == 0 => {
        val playerIndex = (marble - 1) % scores.length
        val currentScore = scores(playerIndex)

        val node = circle.rotate(-7)
        Game(node.removed(), scores.updated(playerIndex, currentScore + marble + node.value))
      }

      case marble => {
        Game(circle.rotate(2).inserted(marble), scores)
      }
    }
  }
}

object Game {
  def apply(numberOfPlayers: Int): Game = {
    new Game(Circle(0), List.fill(numberOfPlayers)(0))
  }
}