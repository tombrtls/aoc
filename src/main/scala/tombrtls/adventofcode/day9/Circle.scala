package tombrtls.adventofcode.day9

case class Circle(marbles: List[Int], activeMarbleIndex: Int) {

  def remove7nthMarbleFromActive: (Int, Circle) = {
    def correctIndex(index: Int): Int = {
      if (index < 0) {
        marbles.length + index
      } else {
        index
      }
    }

    val nextActiveIndex = correctIndex(activeMarbleIndex - 7)

    val value = marbles(nextActiveIndex)
    val circle = this.copy(
      marbles = marbles.take(nextActiveIndex) ++ marbles.drop(nextActiveIndex + 1),
      activeMarbleIndex = nextActiveIndex
    )

    (value, circle)
  }

  def placeMarble(marble: Int): Circle = {
    def correctIndex(index: Int): Int = {
      if (marbles.length <= 1) {
        marbles.length
      } else if (index > marbles.length) {
        index % marbles.length
      } else {
        index
      }
    }

    val nextActiveMarbleIndex = correctIndex(activeMarbleIndex + 2)
    val updatedMarbles = marbles.take(nextActiveMarbleIndex) ++ Seq(marble) ++ marbles.drop(nextActiveMarbleIndex)
    this.copy(updatedMarbles, nextActiveMarbleIndex)
  }
}
