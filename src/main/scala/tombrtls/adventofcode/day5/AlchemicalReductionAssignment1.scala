package tombrtls.adventofcode.day5

import tombrtls.adventofcode.Assignment

import scala.annotation.tailrec

object AlchemicalReductionAssignment1 extends Assignment[String, Int] {
  def main(args: Array[String]): Unit = {
    assert(shouldCharactersBeDestroyed('a', 'A') == true)
    assert(shouldCharactersBeDestroyed('A', 'a') == true)
    assert(shouldCharactersBeDestroyed('a', 'a') == false)
    assert(shouldCharactersBeDestroyed('A', 'A') == false)
    startAssignment
  }

  override val day: Int = 5
  override val testCases: Seq[(String, Int)] = Seq(
    ("sample.txt", 10)
  )

  override val inputFileName: String = "input.txt"

  override def processLines(lines: Seq[String]): String = lines.head

  override def implementation(input: String): Int = {
    removeSimilarCharactersIfAdjescent(input).length
  }

  @tailrec
  def removeSimilarCharactersIfAdjescent(input: String, startIndex: Int = 0): String = {
    val groupedCharacters = input.zip(input.tail)
    val index = groupedCharacters
        .indexWhere( { chars => shouldCharactersBeDestroyed(chars._1, chars._2) }, startIndex)

    index match {
      case -1 => input
      case index => removeSimilarCharactersIfAdjescent(input.take(index) ++ input.drop(index + 2), index - 1)
    }
  }

  private def shouldCharactersBeDestroyed(firstChar: Char, secondChar: Char) = {
    val smallestChar = Math.min(firstChar, secondChar).toChar
    val biggestChar = Math.max(firstChar, secondChar).toChar
    smallestChar.isUpper && smallestChar.toLower == biggestChar
  }
}
