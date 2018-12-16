package tombrtls.adventofcode.day11

import sun.security.x509.SerialNumber
import tombrtls.adventofcode.day11.ChronalChargeHelper.NaivePowerMap
import tombrtls.adventofcode.{Vector2, day11}

import scala.collection.mutable

case class FuelCell(size: Int, powerLevel: Int, coordinate: Vector2)

object ChronalChargeHelper {
  def powerLevel(x: Int, y: Int, serialNumber: Int): Int = {
    val rackId = x + 10
    hundrest(((rackId * y) + serialNumber) * rackId) - 5
  }

  def hundrest(level: Int): Int = {
    val stringValue = s"${level}"
    if (stringValue.length >= 3) {
      stringValue.reverse.substring(2, 3).toInt
    } else {
      0
    }
  }

  def findBiggestPowerLevelGrid(serialNumber: Int): FuelCell =
    findBiggestPowerLevelGrid(serialNumber, createPowerMap(serialNumber))(3)

  def findBiggestPowerLevelGrid(serialNumber: Int, powermap: PowerMap)(cellSize: Int): FuelCell = {
    var largestPower = FuelCell(Int.MinValue, Int.MinValue, Vector2(Int.MinValue, Int.MinValue))
    for (
      x <- 1 until (300 - cellSize);
      y <- 1 until (300 - cellSize)
    ) yield {
      val power = powermap.levelFor(x, y, cellSize)
      if (power > largestPower.powerLevel) {
        largestPower = FuelCell(cellSize, power, Vector2(x, y))
      }
    }

    largestPower
  }

  trait PowerMap {
    def levelFor(x: Int, y: Int, size: Int): Int
  }

  case class PartialSumPowerMap(serialNumber: Int, powerlevelF: (Int, Int, Int) => Int) extends PowerMap {
    private val powerLevels = new Array[Array[Int]](300)
    for (x <- 0 until 300) {
      val innerArray = new Array[Int](300)
      powerLevels.update(x, innerArray)

      for (y <- 0 until 300) {
        val powerLevel =
          levelFor(x - 1, y) + levelFor(x, y - 1) -
          levelFor(x - 1, y - 1) + powerlevelF(x + 1, y + 1, serialNumber)
        innerArray.update(y, powerLevel)
      }
    }

    private def levelFor(x: Int, y: Int): Int = {
      if (x < 0 || y < 0) {
        0
      } else {
        powerLevels(x)(y)
      }
    }

    def levelFor(x: Int, y: Int, size: Int): Int = {
      val minX = x - 2
      val minY = y - 2
      val maxX = x + size - 2
      val maxY = y + size - 2
      val bottomRight = levelFor(maxX, maxY)
      val topRight = levelFor(maxX, minY)
      val bottomLeft = levelFor(minX, maxY)
      val topLeft = levelFor(minX, minY)
      bottomRight - bottomLeft - topRight + topLeft
    }
  }
  
  case class NaivePowerMap(serialNumber: Int, powerlevelF: (Int, Int, Int) => Int) extends PowerMap {
    private val powerLevels = new Array[Array[Int]](300)
    for (x <- 0 until 300) {
      val innerArray = new Array[Int](300)
      for (y <- 0 until 300) {
        innerArray.update(y, powerlevelF(x + 1, y + 1, serialNumber))
      }
      powerLevels.update(x, innerArray)
    }

    def levelFor(x: Int, y: Int, size: Int): Int = {
      var power = 0
      var cellX = x - 1
      var cellY = 0
      while (cellX <= x + size - 2) {
        cellY = y - 1
        while (cellY <= y + size - 2) {
          power += powerLevels(cellX)(cellY)
          cellY += 1
        }

        cellX += 1
      }

      power
    }
  }

  def createPowerMap(serialNumber: Int): PowerMap = {
    createPartialSumPowerMap(serialNumber)
  }

  def createNaivePowerMap(serialNumber: Int): NaivePowerMap = {
    NaivePowerMap(serialNumber, powerLevel)
  }

  def createPartialSumPowerMap(serialNumber: Int): PartialSumPowerMap = {
    PartialSumPowerMap(serialNumber, powerLevel)
  }
}
