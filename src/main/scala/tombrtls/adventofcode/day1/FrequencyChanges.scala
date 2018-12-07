package tombrtls.adventofcode.day1

import tombrtls.adventofcode.Assignment

object FrequencyChanges extends Assignment[Seq[Int], Int] {
  def main(args: Array[String]): Unit = start

  override val day: Int = 1
  override val testCases: Seq[(String, Int)] = Seq(
    ("changes_sample_1.txt", 3),
    ("changes_sample_2.txt", 0),
    ("changes_sample_3.txt", -6),
  )
  override val inputFileName: String = "input.txt"

  override def processLines(lines: Seq[String]): Seq[Int] =
    lines.map(_.toInt)

  override def implementation(input: Seq[Int]): Int =
    input.reduce(_ + _)
}
