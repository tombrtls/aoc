package tombrtls.adventofcode.day6

case class Location(name: String, coordinate: Coordinate) {
  def distance(coordinate: Coordinate) = this.coordinate.distance(coordinate)
}
