package tombrtls.adventofcode.day2

import org.scalatest.{FunSpec, Matchers}

class PackageChecksumTests extends FunSpec with Matchers {
  it("should properly calculate the checksum of the codes") {
    val codes = Seq("abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab")
    PackageChecksum.checksum(codes) should be (12)
  }
}
