package tombrtls.adventofcode.day2



object PackageChecksum {
  def main(args: Array[String]): Unit = {
    print(s"Output: ${checksum(PackageData.packageCodes)}")
  }

  def checksum(codes: Seq[String]): Int = {
    val counts = codes
      .map(countDoublesAndTriples)
      .map {
        case DoublesAndTriples(true, true) => (1, 1)
        case DoublesAndTriples(true, false) => (1, 0)
        case DoublesAndTriples(false, true) => (0, 1)
        case DoublesAndTriples(false, false) => (0, 0)
      }
      .reduce { (counts1, counts2) => (counts1._1 + counts2._1, counts1._2 + counts2._2) }

    counts._1 * counts._2
  }

  def countDoublesAndTriples(code: String): DoublesAndTriples = {
    val charMap = code.toCharArray.groupBy { char => char }.values
    val double = charMap.exists { _.length == 2 }
    val triple = charMap.exists { _.length == 3 }
    DoublesAndTriples(double, triple)
  }
}
