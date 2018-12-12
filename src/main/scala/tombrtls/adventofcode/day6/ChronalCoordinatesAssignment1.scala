package tombrtls.adventofcode.day6

import tombrtls.adventofcode.Assignment

object ChronalCoordinatesAssignment1 extends Assignment[Seq[Location], Int] {
  def main(args: Array[String]): Unit = startAssignment

  override val day: Int = 6
  override val testCases: Seq[(String, Int)] = Seq(
    ("sample.txt", 17)
  )

  override val inputFileName: String = "input.txt"

  private val coordinateRegex = "(\\d*), (\\d*)".r
  override def processLines(lines: Seq[String]): Seq[Location] =
    lines
      .map {
        case coordinateRegex(x, y) => Coordinate(x.toInt, y.toInt)
      }
      .zipWithIndex
      .map { case (coordinate, index) => Location(s"${index}", coordinate) }


  override def implementation(input: Seq[Location]): Int = {
    val maxX = input.maxBy(_.coordinate.x).coordinate.x
    val maxY = input.maxBy(_.coordinate.y).coordinate.y
    val bounds = Bounds(maxX + 1, maxY + 1)

    bounds
      .locationsToClosestCoordinates(input)
      .filter { case (_, coordinates) => coordinates.exists(bounds.isOnTheEdge) == false }
      .mapValues { _.length }
      .values
      .max
  }

}
