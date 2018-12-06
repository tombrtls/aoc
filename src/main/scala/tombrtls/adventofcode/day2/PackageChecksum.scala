package tombrtls.adventofcode.day2

import tombrtls.adventofcode.FileHelper

object PackageChecksum {
  val input = FileHelper.readLines("/day2/input.txt")
  def main(args: Array[String]): Unit = {
    print(s"Output: ${checksum(input)}")
  }

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
