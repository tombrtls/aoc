package tombrtls.adventofcode.day3

case class Square(id: Int, x: Int, y: Int, width: Int, height: Int) {
  val horizontalRange = x until (x + width)
  val verticalRange = y until (y + height)


  def intersectsWith(that: Square): Boolean =
    this.horizontalRange.intersect(that.horizontalRange).length > 0 &&
      this.verticalRange.intersect(that.verticalRange).length > 0
}

object Square {
  def intersectsWith(square1: Square)(square2: Square): Boolean =
    square1.intersectsWith(square2)
}