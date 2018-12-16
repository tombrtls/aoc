package tombrtls.adventofcode.day12

import tombrtls.adventofcode.Assignment

import scala.annotation.tailrec
import scala.collection.mutable

case class Pots(plants: List[Boolean], startingIndex: Long) {
  lazy val score = {
    var score: Long = 0
    for ((plant, index) <- plants.zipWithIndex) {
      if (plant == true) {
        score += index + startingIndex
      }
    }
    score
  }

  def processV2(rules: Seq[Rule]): Pots = {
    val updatedPlants = new Array[Boolean](plants.length + 6)
    (0 until plants.length).foreach(updatedPlants.update(_, false))
    val getPlant = plants.lift.andThen(_.getOrElse(false))

    var firstIndex = Int.MaxValue
    var lastIndex = Int.MinValue
    for (
      rule <- rules if (rule.result);
      index <- -3 until plants.length + 3
    ) {
      if (rule.criteria(0) == getPlant(index) &&
        rule.criteria(1) == getPlant(index + 1) &&
        rule.criteria(2) == getPlant(index + 2) &&
        rule.criteria(3) == getPlant(index + 3) &&
        rule.criteria(4) == getPlant(index + 4)) {
        if (rule.result) {
          updatedPlants.update(index + 3, rule.result)
          firstIndex = Math.min(firstIndex, index)
          lastIndex = Math.max(lastIndex, index)
        }
      }
    }

    Pots(updatedPlants.slice(firstIndex + 3, lastIndex + 4).toList, this.startingIndex + firstIndex + 2)
  }
}
case class Rule(criteria: Array[Boolean], result: Boolean) {
  def appliesTo(plants: Seq[Boolean]) = {
    criteria.zip(plants).forall { case (criteria, plant) => criteria == plant }
  }
}

object SubterraneanSustainabilityAssignment1 extends Assignment[(Pots, Seq[Rule]), BigInt] {
  def main(args: Array[String]): Unit = startAssignment

  override val day: Int = 12
  override val testCases: Seq[(String, BigInt)] = Seq(
    ("sample_0.txt", 325)
  )

  override val inputFileName: String = "input.txt"

  private val initialStateRegex = "initial state: (.*)".r
  private val ruleRegex = "(.*) => (.)".r
  override def processLines(lines: Seq[String]): (Pots, Seq[Rule]) = {
    val plants = lines.head match {
      case initialStateRegex(plantStates) => stringToBooleans(plantStates)
    }

    val rules = lines.drop(2).map {
      case ruleRegex(criteriaString, resultString) => {
        Rule(stringToBooleans(criteriaString).toArray, charToBool(resultString.head))
      }
    }

    (Pots(plants.toList, 0), rules)
  }

  def stringToBooleans(string: String): Seq[Boolean] = string.map(charToBool)
  def charToBool(char: Char): Boolean = char match {
    case '.' => false
    case '#' => true
  }

  override def implementation(input: (Pots, Seq[Rule])): BigInt = {
    val (pots, rules) = input

    val lastPot = (0 until 20).foldLeft(pots) { (pot, _) => pot.processV2(rules) }

    processMultipleGenerations(pots, rules, 50000000000l)
  }

  def processMultipleGenerations(pots: Pots, rules: Seq[Rule], numberOfGenerations: Long): BigInt = {

    val testRange = 1000
    val allPots = (0 until testRange).foldLeft(Seq(pots)) { (pots, _) => pots :+ pots.last.processV2(rules) }
    val indices = allPots.drop(1).zip(allPots).zipWithIndex.collect {
      case ((a, b), ind) if a.plants == b.plants => ind
    }

    val index = indices.last
    val secondLastPot = allPots(index - 1)
    val lastPot = allPots(index)

    val offsetDiff = lastPot.startingIndex - secondLastPot.startingIndex
    val timeDifference = numberOfGenerations - index
    Pots(lastPot.plants, lastPot.startingIndex + (offsetDiff * timeDifference)).score
  }
}
