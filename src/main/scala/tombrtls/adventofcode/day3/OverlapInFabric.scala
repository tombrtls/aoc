package tombrtls.adventofcode.day3

import tombrtls.adventofcode.{Assignment, FileHelper}

object OverlapInFabric extends Assignment[Seq[Square], Int] {

  def main(args: Array[String]): Unit = {
    verify()
    run()
  }

  override val sampleInputFile: String = "/day3/sample.txt"
  override val sampleExpectation: Int = 2
  override val inputFile: String = "/day3/input.txt"

  private val squarePattern = "#(\\d) @ (\\d),(\\d): (\\d)x(\\d)".r
  override def processLines(lines: Seq[String]): Seq[Square] =
    lines.map { line =>
      line match {
        case squarePattern(id, startIndex, endIndex, width, height) =>
          Square(id.toInt, startIndex.toInt, endIndex.toInt, width.toInt, height.toInt)
      }
    }

  override def implementation(input: Seq[Square]): Int = {

    0
  }
}
