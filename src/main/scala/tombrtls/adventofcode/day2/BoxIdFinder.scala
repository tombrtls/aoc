package tombrtls.adventofcode.day2

import tombrtls.adventofcode.{Assignment, FileHelper}

object BoxIdFinder extends Assignment[Seq[String], String]{
  def main(args: Array[String]): Unit = startAssignment

  override val day: Int = 2
  override val testCases = Seq(
    ("sample.txt", "fgij")
  )
  override val inputFileName: String = "input.txt"

  override def processLines(lines: Seq[String]): Seq[String] = lines

  override def implementation(input: Seq[String]): String = {
    val (boxId1, boxId2) = findSimilarBoxIds(input)
    common(boxId1, boxId2)
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