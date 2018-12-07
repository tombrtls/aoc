package tombrtls.adventofcode.day3

import tombrtls.adventofcode.Assignment

object OverlapInFabric extends Assignment[Seq[Square], Int] {
  def main(args: Array[String]): Unit = startAssignment

  override val day: Int = 3
  override val testCases = Seq(
    ("sample.txt", 4)
  )
  override val inputFileName: String = "input.txt"

  private val squarePattern = "#(\\d*) @ (\\d*),(\\d*): (\\d*)x(\\d*)".r
  override def processLines(lines: Seq[String]): Seq[Square] =
    lines.map { line =>
      line match {
        case squarePattern(id, x, y, width, height) =>
          Square(id.toInt, x.toInt, y.toInt, width.toInt, height.toInt)
      }
    }

  override def implementation(input: Seq[Square]): Int = tilesOverlap(input)

  def tilesOverlap(squares: Seq[Square]): Int =
    squares.flatMap(squaresToCoordinates)
      .groupBy { coordinates => coordinates }
      .count { case (_, items) => items.length > 1 }

  def squaresToCoordinates(square: Square): Seq[Coordinates] = {
    for (
      x <- square.horizontalRange;
      y <- square.verticalRange
    ) yield Coordinates(x, y)
  }
}
