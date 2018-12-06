package tombrtls.adventofcode.day3

import tombrtls.adventofcode.Assignment

case class Coordinates(x: Int, y: Int)
case class Square(id: Int, x: Int, y: Int, width: Int, height: Int) {
  val horizontalRange = x until (x + width)
  val verticalRange = y until (y + height)
}

object OverlapInFabric extends Assignment[Seq[Square], Int] {
  def main(args: Array[String]): Unit = start

  override val sampleInputFile: String = "/day3/sample.txt"
  override val sampleExpectation: Int = 4
  override val inputFile: String = "/day3/input.txt"

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
