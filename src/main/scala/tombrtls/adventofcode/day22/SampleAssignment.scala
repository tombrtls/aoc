package tombrtls.adventofcode.day22

import tombrtls.adventofcode.Assignment

object SampleAssignment extends Assignment[Seq[String], Int] {
  def main(args: Array[String]): Unit = startAssignment

  override val day: Int = 1
  override val testCases: Seq[(String, Int)] = Seq(
    ("sample_1.txt", 0),
    ("sample_2.txt", 0)
  )
  override val inputFileName: String = "input.txt"

  override def processLines(lines: Seq[String]): Seq[String] = ???

  override def implementation(input: Seq[String]): Int = ???
}
