package tombrtls.adventofcode.day11

import tombrtls.adventofcode.day11.ChronalChargeHelper._
import tombrtls.adventofcode.{Assignment, Vector2}

object ChronalChargeAssignment1 extends Assignment[Int, FuelCell] {
  def main(args: Array[String]): Unit = startAssignment

  override val day: Int = 11
  override val testCases: Seq[(String, FuelCell)] = Seq()
  override val inputFileName: String = "input.txt"

  override def processLines(lines: Seq[String]): Int = lines.head.toInt

  override def implementation(input: Int): FuelCell = {
    findBiggestPowerLevelGrid(input)
  }
}
