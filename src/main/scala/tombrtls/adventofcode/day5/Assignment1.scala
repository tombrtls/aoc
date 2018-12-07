package tombrtls.adventofcode.day5

import tombrtls.adventofcode.Assignment

object SampleAssignment extends Assignment[Seq[String], Int] {
  def main(args: Array[String]): Unit = startAssignment

  override val day: Int = 5
  override val testCases: Seq[(String, Int)] = Seq(
    ("sampletxt", 0)
  )

  override val inputFileName: String = "input.txt"

  override def processLines(lines: Seq[String]): Seq[String] = ???

  override def implementation(input: Seq[String]): Int = ???
}
