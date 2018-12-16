package tombrtls.adventofcode.day1171

import tombrtls.adventofcode.day11.ChronalChargeAssignment2._
import tombrtls.adventofcode.day11.ChronalChargeHelper._
import org.scalatest.{FunSpec, Matchers}
import tombrtls.adventofcode.day11.FuelCell
import tombrtls.adventofcode.{Vector2, day11}

import scala.None

class ChronalChargeAssignmentTests extends FunSpec with Matchers {
  describe("ChronalChargeHelper") {
    it("should properly calculate the power levels") {
      powerLevel(122, 79, 57) should equal(-5)
      powerLevel(217, 196, 39) should equal(0)
      powerLevel(101, 153, 71) should equal(4)
    }

    it("should return the 100th value from the int") {
      hundrest(100) should equal(1)
      hundrest(123) should equal(1)
      hundrest(199) should equal(1)
      hundrest(99199) should equal(1)
      hundrest(99399) should equal(3)
      hundrest(12456789) should equal(7)
      hundrest(12) should equal(0)
    }

    it("should return the coordinates and grid size of the ") {
      findBiggestPowerLevelGrid(18) should equal (FuelCell(3, 29, Vector2(33, 45)))
      findBiggestPowerLevelGrid(42) should equal (FuelCell(3, 30, Vector2(21, 61)))
      findBiggestPowerLevelGrid(9005) should equal (FuelCell(3, 31, Vector2(20, 32)))
    }
  }

  describe("NaivePowerMap") {
    it("should generate a proper powermap") {
      val powerMap = createPowerMap(23)
      powerMap.levelFor(1, 1, 1) should equal (powerLevel(1, 1, 23))
      powerMap.levelFor(2, 3, 1) should equal (powerLevel(2, 3, 23))
      powerMap.levelFor(2, 4, 1) should equal (powerLevel(2, 4, 23))
      powerMap.levelFor(2, 2, 1) should equal (powerLevel(2, 2, 23))

      powerMap.levelFor(2, 2, 1) should equal (powerLevel(2, 2, 23))
      powerMap.levelFor(3, 3, 1) should equal (powerLevel(3, 3, 23))
      powerMap.levelFor(4, 4, 1) should equal (powerLevel(4, 4, 23))

      powerMap.levelFor(300, 300, 1) should equal (powerLevel(300, 300, 23))
    }

    it("should be able to calculate the powerlevel for a sub section of the powermap") {
      val powermap = createPowerMap(23)
      def power(x: Int, y: Int): Int = powermap.levelFor(x, y, 1)

      val size2Power =
        power(1, 1) + power(2, 1) +
          power(1, 2) + power(2, 2)

      powermap.levelFor(1, 1, 2) should equal (size2Power)

      val size3Power =
        power(1, 1) + power(2, 1) + power(3, 1) +
          power(1, 2) + power(2, 2) + power(3, 2) +
          power(1, 3) + power(2, 3) + power(3, 3)

      powermap.levelFor(1, 1, 3) should equal (size3Power)
    }
  }

  describe("PartialSumPowerMap") {
    it("should generate a proper powermap") {
      val powerMap = createPartialSumPowerMap(23)
      powerMap.levelFor(1, 1, 1) should equal (powerLevel(1, 1, 23))
      powerMap.levelFor(2, 2, 1) should equal (powerLevel(2, 2, 23))
      powerMap.levelFor(2, 3, 1) should equal (powerLevel(2, 3, 23))
      powerMap.levelFor(2, 4, 1) should equal (powerLevel(2, 4, 23))
      powerMap.levelFor(2, 2, 1) should equal (powerLevel(2, 2, 23))

      powerMap.levelFor(2, 2, 1) should equal (powerLevel(2, 2, 23))
      powerMap.levelFor(3, 3, 1) should equal (powerLevel(3, 3, 23))
      powerMap.levelFor(4, 4, 1) should equal (powerLevel(4, 4, 23))

      powerMap.levelFor(300, 300, 1) should equal (powerLevel(300, 300, 23))
    }

    it("should be able to calculate the powerlevel for a sub section of the powermap") {
      val powermap = createPowerMap(23)
      def power(x: Int, y: Int): Int = powermap.levelFor(x, y, 1)

      val size2Power =
        power(1, 1) + power(2, 1) +
          power(1, 2) + power(2, 2)

      powermap.levelFor(1, 1, 2) should equal (size2Power)

      val size3Power =
        power(1, 1) + power(2, 1) + power(3, 1) +
          power(1, 2) + power(2, 2) + power(3, 2) +
          power(1, 3) + power(2, 3) + power(3, 3)

      powermap.levelFor(1, 1, 3) should equal (size3Power)
    }
  }

  describe("ChronalChargeAssignment1") {
    import tombrtls.adventofcode.day11.ChronalChargeAssignment1._
    it("should return the coordinates of the grid with the most power") {
      implementation(18) should equal(FuelCell(3, 29, Vector2(33, 45)))
      implementation(42) should equal(FuelCell(3, 30, Vector2(21, 61)))
      implementation(9005) should equal(FuelCell(3, 31, Vector2(20, 32)))
    }
  }

  describe("ChronalChargeAssignment2") {
    import tombrtls.adventofcode.day11.ChronalChargeAssignment2._
    it("should be able to identify the size of the grid that provides most power 1") {
      implementation(18) should equal (FuelCell(16, 113, Vector2(90, 269)))
    }

    it("should be able to identify the size of the grid that provides most power 2") {
      implementation(42) should equal (FuelCell(12, 119, Vector2(232, 251)))
    }

    it("should calculate everything") {
      implementation(9005) should equal (FuelCell(13, 148, Vector2(235, 287)))
    }
  }
}
