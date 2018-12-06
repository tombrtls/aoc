package tombrtls.adventofcode.day2

import tombrtls.adventofcode.day2.BoxIdFinder.start
import tombrtls.adventofcode.{Assignment, FileHelper}

object PackageChecksum extends Assignment[Seq[String], Int] {
  def main(args: Array[String]): Unit = start

  override val sampleInputFile: String = "/day2/sample.txt"
  override val sampleExpectation: Int = 2
  override val inputFile: String = "/day2/input.txt"

  override def processLines(lines: Seq[String]): Seq[String] = lines

  override def implementation(input: Seq[String]): Int = checksum(input)

  def checksum(codes: Seq[String]): Int = {
    val doublesAndTripleChecks = for {
      code <- codes;
      charCounts = code.groupBy(character).values;
      hasDouble = charCounts.exists(withLengthOf(2));
      hasTriple = charCounts.exists(withLengthOf(3))
    } yield Pair(hasDouble, hasTriple)

    val (doubleChecks, tripleChecks) = doublesAndTripleChecks.unzip
    val numberOfDoubles = doubleChecks.filter(_ == true)
    val numberOfTriples = tripleChecks.filter(_ == true)

    numberOfDoubles.length * numberOfTriples.length
  }

  private def character(char: Char): Char = char

  private def withLengthOf(length: Int)(string: String) = string.length == length
}
