package tombrtls.adventofcode.day2

import org.scalatest.{FunSpec, Matchers}

class PackageChecksumTests extends FunSpec with Matchers {
  it("should properly calculate the checksum of the codes") {
    val codes = Seq("abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab")
    PackageChecksum.checksum(codes) should be (12)
  }

  it("should properly count all doubles and triples") {
    PackageChecksum.count("abcdef") should be (0, 0)
    PackageChecksum.count("bababc") should be (1, 1)
    PackageChecksum.count("abbcde") should be (1, 0)
    PackageChecksum.count("abcccd") should be (0, 1)
    PackageChecksum.count("aabcdd") should be (1, 0)
    PackageChecksum.count("abcdee") should be (1, 0)
    PackageChecksum.count("ababab") should be (0, 1)
  }
}
