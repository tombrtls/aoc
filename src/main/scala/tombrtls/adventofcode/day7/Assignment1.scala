package tombrtls.adventofcode.day7

import tombrtls.adventofcode.Assignment

object Assignment1 extends Assignment[Seq[String], Int] {
  def main(args: Array[String]): Unit = startAssignment

  override val day: Int = 7
  override val testCases: Seq[(String, Int)] = Seq(
    ("sampletxt", 0)
  )

  override val inputFileName: String = "input.txt"

  override def processLines(lines: Seq[String]): Seq[String] = ???

  override def implementation(input: Seq[String]): Int = ???
}
