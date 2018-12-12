package tombrtls.adventofcode.day5

import tombrtls.adventofcode.Assignment

import scala.annotation.tailrec

object AlchemicalReductionAssignment2 extends Assignment[String, Int] {
  def main(args: Array[String]): Unit = {
    assert(shouldCharactersBeDestroyed('a', 'A') == true)
    assert(shouldCharactersBeDestroyed('A', 'a') == true)
    assert(shouldCharactersBeDestroyed('a', 'a') == false)
    assert(shouldCharactersBeDestroyed('A', 'A') == false)
    startAssignment
  }

  override val day: Int = 5
  override val testCases: Seq[(String, Int)] = Seq(
    ("sample.txt", 4)
  )

  override val inputFileName: String = "input.txt"

  override def processLines(lines: Seq[String]): String = lines.head

  val units = 'a' until 'z'

  override def implementation(input: String): Int = {
    val processedInput = for (
      unit <- units;
      strippedInput = removeUnit(input)(unit) if strippedInput.length != input.length;
      reactedInput = removeSimilarCharactersIfAdjescent(strippedInput)
    ) yield {
      (unit, reactedInput)
    }

    processedInput
      .foreach { case (unit, length) => println(s"Unit ${unit} results in ${length.length}"); }

    processedInput
      .map { case (unit, output) => output.length }
      .min
  }

  def removeUnit(string: String)(unit: Char): String = {
    val regex = s"([${unit}${unit.toUpper}])".r
    regex.replaceAllIn(string, "")
  }

  @tailrec
  def removeSimilarCharactersIfAdjescent(input: String): String = {

    val potentialIndex = 0.until(input.length - 1)
      .find { index =>
        val firstChar = input(index);
        val secondChar = input(index + 1);
        shouldCharactersBeDestroyed(firstChar, secondChar)
      }

    potentialIndex match {
      case None => input
      case Some(index) => {
        removeSimilarCharactersIfAdjescent(input.take(index) ++ input.drop(index + 2))
      }
    }
  }

  private def shouldCharactersBeDestroyed(firstChar: Char, secondChar: Char) = {
    val smallestChar = Math.min(firstChar, secondChar).toChar
    val biggestChar = Math.max(firstChar, secondChar).toChar
    smallestChar.isUpper && smallestChar.toLower == biggestChar
  }
}
