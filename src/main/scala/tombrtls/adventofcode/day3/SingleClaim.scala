package tombrtls.adventofcode.day3

import tombrtls.adventofcode.Assignment

object SingleClaim extends Assignment[Seq[Square], Int] {
  def main(args: Array[String]): Unit = startAssignment

  override val day: Int = 3
  override val testCases = Seq(
    ("sample.txt", 3)
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

  override def implementation(input: Seq[Square]): Int = findSquareWithoutOverlap(input)

  def findSquareWithoutOverlap(squares: Seq[Square]): Int = {
    val square = squares
      .find { square =>
        squares.count(square.intersectsWith) <= 1
      }

    square.get.id
  }
}
