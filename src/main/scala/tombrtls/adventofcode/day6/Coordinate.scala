package tombrtls.adventofcode.day6

case class Coordinate(x: Int, y: Int) {
  def distance(that: Coordinate): Int =
    Math.abs(that.x - this.x) + Math.abs((that.y - this.y))

  def closestLocation(locations: Seq[Location]): Option[Location] = {
    val coordinatesAndDistance = for (
      location <- locations;
      distance = location.distance(this)
    ) yield (location, distance)

    val maxDistance = coordinatesAndDistance.minBy(_._2)._2
    val closestCoordinates = coordinatesAndDistance.filter(_._2 == maxDistance)
    closestCoordinates match {
      case (coordinate, _) +: Nil => Some(coordinate)
      case _ => None
    }
  }
}
