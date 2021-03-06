package tombrtls.adventofcode.day8

import tombrtls.adventofcode.Assignment

object MemoryManeuverAssignment1 extends Assignment[Seq[Int], Int] {
  def main(args: Array[String]): Unit = startAssignment

  override val day: Int = 8
  override val testCases: Seq[(String, Int)] = Seq(
    ("sample.txt", 138)
  )

  override val inputFileName: String = "input.txt"

  override def processLines(lines: Seq[String]): Seq[Int] =
    lines.head.split(" ").map(_.toInt)

  override def implementation(input: Seq[Int]): Int = {
    Node(input).metadataCombined
  }
}
