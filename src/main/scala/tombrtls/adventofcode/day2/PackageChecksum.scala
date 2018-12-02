package tombrtls.adventofcode.day2

object PackageChecksum {
  def main(args: Array[String]): Unit = {
    print(s"Output: ${checksum(PackageData.packageCodes)}")
  }

  def checksum(codes: Seq[String]): Int = {
    val counts = codes
      .map(count)
      .reduce { (count1, count2) =>
        (count1._1 + count2._1, count1._2 + count2._2)
      }

    counts._1 * counts._2
  }

  def count(code: String): (Int, Int) = {
    val charMap = code.toCharArray.groupBy { char => char }.values
    val double: Int = Math.min(charMap.count { _.length == 2 }, 1)
    val triple: Int = Math.min(charMap.count { _.length == 3 }, 1)
    (double, triple)
  }
}
