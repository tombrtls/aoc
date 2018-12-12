package tombrtls.adventofcode.day6

case class Bounds(width: Int, height: Int) {
  val horizontalRange = 0 until width
  val verticalRange = 0 until height

  val allCoordinates: Seq[Coordinate] = {
    for (
      x <- this.horizontalRange;
      y <- this.verticalRange
    ) yield Coordinate(x, y)
  }

  def isCoordinateInside(coordinate: Coordinate) = {
    horizontalRange.contains(coordinate.x) && verticalRange.contains(coordinate.y)
  }

  def isOnTheEdge(coordinate: Coordinate): Boolean = {
    coordinate.x == 0 || coordinate.y == 0 || coordinate.x == width - 1 || coordinate.y == height - 1
  }

  def locationsToClosestCoordinates(locations: Seq[Location]): Map[Location, Seq[Coordinate]] = {
    val locationAndCoordinate = for (
      coordinate <- this.allCoordinates;
      closestLocation <- coordinate.closestLocation(locations)
    ) yield (closestLocation, coordinate)

    locationAndCoordinate
      .groupBy { _._1 }
      .mapValues { _.map(_._2) }
  }
}
