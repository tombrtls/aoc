package tombrtls.adventofcode.day5

import tombrtls.adventofcode.Assignment

import scala.annotation.tailrec

object AlchemicalReductionAssignment extends Assignment[String, Int] {
  def main(args: Array[String]): Unit = {
    assert(shouldCharactersDestroy('a', 'A') == true)
    assert(shouldCharactersDestroy('A', 'a') == true)
    assert(shouldCharactersDestroy('a', 'a') == false)
    assert(shouldCharactersDestroy('A', 'A') == false)
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
  def removeSimilarCharactersIfAdjescent(input: String): String = {
    val groupedCharacters = input.zip(input.tail)
    val index = groupedCharacters
      .indexWhere { chars => shouldCharactersDestroy(chars._1, chars._2) }

    index match {
      case -1 => input
      case index => removeSimilarCharactersIfAdjescent(input.take(index) ++ input.drop(index + 2))
    }
  }

  private def shouldCharactersDestroy(firstChar: Char, seconDhar: Char) = {
    val smallestChar = Math.min(firstChar, seconDhar).toChar
    val biggestChar = Math.max(firstChar, seconDhar).toChar
    smallestChar.isUpper && smallestChar.toLower == biggestChar
  }
}
