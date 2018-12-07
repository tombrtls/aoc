package tombrtls.adventofcode.day2

import tombrtls.adventofcode.Assignment

object PackageChecksum extends Assignment[Seq[String], Int] {
  def main(args: Array[String]): Unit = start

  override val day: Int = 2
  override val testCases = Seq(
    ("sample.txt", 2)
  )
  override val inputFileName: String = "input.txt"

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
