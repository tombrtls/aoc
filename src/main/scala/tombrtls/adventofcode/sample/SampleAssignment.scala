package tombrtls.adventofcode.sample

import tombrtls.adventofcode.Assignment

object SampleAssignment extends Assignment[Seq[String], Int] {
  override val day: Int = 1
  override val testCases: Seq[(String, Int)] = Seq()
  override val inputFileName: String = "input.txt"

  override def processLines(lines: Seq[String]): Seq[String] = ???

  override def implementation(input: Seq[String]): Int = ???
}
