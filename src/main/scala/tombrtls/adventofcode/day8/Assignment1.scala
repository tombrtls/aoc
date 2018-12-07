package tombrtls.adventofcode.day8

import tombrtls.adventofcode.Assignment

object Assignment1 extends Assignment[Seq[String], Int] {
  def main(args: Array[String]): Unit = startAssignment

  override val day: Int = 8
  override val testCases: Seq[(String, Int)] = Seq(
    ("sample.txt", 0)
  )

  override val inputFileName: String = "input.txt"

  override def processLines(lines: Seq[String]): Seq[String] = ???

  override def implementation(input: Seq[String]): Int = ???
}
