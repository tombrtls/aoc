package tombrtls.adventofcode.day6

import tombrtls.adventofcode.Assignment

case class Coordinate(x: Int, y: Int) {
  def distance(that: Coordinate): Int =
    Math.abs(that.x - this.x) + Math.abs((that.y - this.y))
}

object ChronalCoordinatesAssignment1 extends Assignment[Seq[Coordinate], Int] {
  def main(args: Array[String]): Unit = startAssignment

  override val day: Int = 6
  override val testCases: Seq[(String, Int)] = Seq(
    ("sample.txt", 17)
  )

  override val inputFileName: String = "input.txt"

  private val coordinateRegex = "(\\d*), (\\d*)".r
  override def processLines(lines: Seq[String]): Seq[Coordinate] =
    lines
      .map {
        case coordinateRegex(x, y) => Coordinate(x.toInt, y.toInt)
      }

  override def implementation(input: Seq[Coordinate]): Int = {
    val minX = input.minBy(_.x).x
    val maxX = input.maxBy(_.x).x
    val minY = input.minBy(_.y).y
    val maxY = input.maxBy(_.y).y

    val finiteSpacelocations = input.filter { location =>
      (minX until maxX contains location.x) &&
        (minY until maxY contains location.y)
    }

    val cloestLocations = for (
      x <- 0 until maxX;
      y <- 0 until maxY;
      coordinate = Coordinate(x, y);
      closestLocation <- closestLocation(coordinate, input)
    ) yield closestLocation

    cloestLocations
      .groupBy { location => location }
      .filterKeys(finiteSpacelocations.contains)
      .mapValues(_.length)
      .values
      .max
  }

  def closestLocation(coordinate: Coordinate, locations: Seq[Coordinate]): Option[Coordinate] = {
    val coordinatesAndDistance = for (
      otherCoordinate <- locations;
      distance = coordinate.distance(otherCoordinate)
    ) yield (otherCoordinate, distance)

    val maxDistance = coordinatesAndDistance.minBy(_._2)._2
    val closestCoordinates = coordinatesAndDistance.filter(_._2 == maxDistance)
    closestCoordinates match {
      case (coordinate, _) +: Nil => Some(coordinate)
      case _ => None
    }
  }
}
