package tombrtls.adventofcode.day12

import org.scalatest.{FunSpec, Matchers}

class SubterraneanSustainabilityTests extends FunSpec with Matchers {
  describe("Pots") {
    it("should be able to detect a rule and apply it if it's not") {
      val pots = Pots(List(true), 0)
      val rule = Seq(false, false, true, false, false).toArray
      val rules = Seq(Rule(rule, true))

      val newPots = pots.processV2(rules)
      newPots.plants.length should equal (1)
      newPots.plants(0) should equal (true)
      newPots.startingIndex should equal (0)
    }

    it("should be able to detect the first rule and apply it") {
      val pots = Pots(List(true), 0)
      val rule = Seq(false, false, true, false, false).toArray
      val rules = Seq(Rule(rule, false))

      val newPots = pots.processV2(rules)
      newPots.plants.length should equal (0)
      newPots.startingIndex should equal (0)
    }

    it("should be able to skip over a bunch of rules before finding the right one") {
      val pots = Pots(List(true), 0)
      val fakeRule = Rule(Seq(true, true, true, true, true).toArray, false)
      val rule = Rule(Seq(false, false, true, false, false).toArray, true)
      val rules = Seq(fakeRule, fakeRule, rule)

      val newPots = pots.processV2(rules)
      newPots.plants.length should equal (1)
      newPots.plants(0) should equal (true)
      newPots.startingIndex should equal (0)
    }

    it("should decrease the startingIndex if applicable") {
      val pots = Pots(List(true), 0)
      val rule = Rule(Seq(false, false, false, true, false).toArray, true)
      val rules = Seq(rule)

      val newPots = pots.processV2(rules)
      newPots.plants.length should equal (1)
      newPots.plants(0) should equal (true)
      newPots.startingIndex should equal (-1)
    }

    it("should increase the startingIndex if applicable") {
      val pots = Pots(List(true), 0)
      val rule = Rule(Seq(false, true, false, false, false).toArray, true)
      val rules = Seq(rule)

      val newPots = pots.processV2(rules)
      newPots.plants.length should equal (1)
      newPots.plants(0) should equal (true)
      newPots.startingIndex should equal (1)
    }

    it("should spawn new plants if applicable") {
      val pots = Pots(List(true), 0)
      val rule1 = Rule(Seq(false, true, false, false, false).toArray, true)
      val rule2 = Rule(Seq(false, false, true, false, false).toArray, false)
      val rule3 = Rule(Seq(false, false, false, true, false).toArray, true)
      val rules = Seq(rule1, rule2, rule3)

      val newPots = pots.processV2(rules)
      newPots.plants.length should equal (3)
      newPots.plants(0) should equal (true)
      newPots.plants(1) should equal (false)
      newPots.plants(2) should equal (true)
      newPots.startingIndex should equal (-1)
    }

    it("everything should die when no rules are defined") {
      val pots = Pots(List(true), 0)
      val rule1 = Rule(Seq(false, true, false, false, false).toArray, true)
      val rule2 = Rule(Seq(false, false, false, true, false).toArray, true)
      val rules = Seq(rule1, rule2)

      val newPots = pots.processV2(rules)
      newPots.plants.length should equal (3)
      newPots.plants(0) should equal (true)
      newPots.plants(1) should equal (false)
      newPots.plants(2) should equal (true)
      newPots.startingIndex should equal (-1)
    }

    it("should calculate the proper score") {
      val plants = "#....##....#####...#######....#.#..##"
        .map {
          case '.' => false
          case '#' => true
        }

      val pots = Pots(plants.toList, -2)
      pots.score should equal (325)
    }

    def stringToBooleans(string: String): List[Boolean] = (string.map {
      case '.' => false
      case '#' => true
    }).toList

    val rules = Seq("...##", "..#..", ".#...", ".#.#.", ".#.##", ".##..", ".####", "#.#.#", "#.###", "##.#.", "##.##", "###..", "###.#", "####.")
        .map(stringToBooleans)
        .map { criteria =>
          Rule(criteria.toArray, true)
        }

    val generations = Seq(
      Pots(stringToBooleans("#..#.#..##......###...###"), 0),
      Pots(stringToBooleans("#...#....#.....#..#..#..#"), 0),
      Pots(stringToBooleans("##..##...##....#..#..#..##"), 0),
      Pots(stringToBooleans("#.#...#..#.#....#..#..#...#"), -1),
      Pots(stringToBooleans("#.#..#...#.#...#..#..##..##"), 0)
    )

    it ("should properly progress from one step to the other") {
      generations(0).processV2(rules) should equal (generations(1))
      generations(1).processV2(rules) should equal (generations(2))
      generations(2).processV2(rules) should equal (generations(3))
      generations(3).processV2(rules) should equal (generations(4))
    }

    it ("properly grow all the way through") {
      val expectedEndResult = Pots(stringToBooleans("#....##....#####...#######....#.#..##"), -2)
      val endResult = (0 until 20).foldLeft(generations(0)) { (pot, _) => pot.processV2(rules) }

      endResult should equal (expectedEndResult)
    }
  }
}
