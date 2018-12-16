package tombrtls.adventofcode.day11


import tombrtls.adventofcode.{Assignment, Vector2}
import tombrtls.adventofcode.day11.ChronalChargeHelper._

import scala.collection.mutable

object ChronalChargeAssignment2 extends Assignment[Int, FuelCell] {
  def main(args: Array[String]): Unit = startAssignment

  override val day: Int = 11
  override val testCases: Seq[(String, FuelCell)] = Seq()
  override val inputFileName: String = "input.txt"

  override def processLines(lines: Seq[String]): Int = lines.head.toInt

  override def implementation(input: Int): FuelCell = {
    val powermap = createPowerMap(input)
    var biggestFuelCell = FuelCell(0, 0, Vector2(0, 0))
    for (
      s <- 1 until 300;
      x <- 1 to 300 - s;
      y <- 1 to 300 - s;
      powerLevel = powermap.levelFor(x, y, s)
      if (powerLevel > biggestFuelCell.powerLevel)
    ) {
      biggestFuelCell = FuelCell(s, powerLevel, Vector2(x, y))
    }

    biggestFuelCell
  }
}

