package tombrtls.adventofcode.day6

import tombrtls.adventofcode.Assignment

object ChronalCoordinatesAssignment2 extends Assignment[Seq[Location], Int] {
  def main(args: Array[String]): Unit = startAssignment

  override val day: Int = 6
  override val testCases: Seq[(String, Int)] = Seq(
    ("sample.txt", 16)
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

    val locationAndDistance = for (
      coordinate <- bounds.allCoordinates;
      location <- input;
      distance = location.distance(coordinate)
    ) yield (coordinate, distance)

    val locationsToTotalDistance = locationAndDistance
      .foldLeft(Map[Coordinate, Int]()) { (acc, locationAndDistance) =>
        val distance = acc.getOrElse(locationAndDistance._1, 0)
        acc.updated(locationAndDistance._1, distance + locationAndDistance._2)
      }

    locationsToTotalDistance
      .count(_._2 < 10000)

//    val maxDistance = locationsToTotalDistance
//      .values
//      .max

//    locationsToTotalDistance.count { case (_, distance) => distance == maxDistance}
  }
}
