package tombrtls.adventofcode.day2

import tombrtls.adventofcode.FileHelper

object BoxIdFinder {

  val input = FileHelper.readLines("/day2/input.txt")
  def main(args: Array[String]): Unit = {
    val similarBoxIds = findSimilarBoxIds(input)
    print(s"Output: ${common(similarBoxIds._1, similarBoxIds._2)}")
  }

  def findSimilarBoxIds(boxIds: Seq[String]): (String, String) = {
    val similarPairs = for(
      box1 <- boxIds;
      box2 <- boxIds if (hasSingleCharacterDifference(box1, box2))
    ) yield (box1, box2)

    similarPairs.head
  }

  def hasSingleCharacterDifference(string1: String, string2: String): Boolean =
    string1.zip(string2).count { (chars) => chars._1 != chars._2 } == 1

  def common(string1: String, string2: String): String =
    string1.intersect(string2)
}