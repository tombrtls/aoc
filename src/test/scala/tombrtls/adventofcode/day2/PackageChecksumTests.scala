package tombrtls.adventofcode.day2

import org.scalatest.{FunSpec, Matchers}

class PackageChecksumTests extends FunSpec with Matchers {
  it("should properly calculate the checksum of the codes") {
    val codes = Seq("abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab")
    PackageChecksum.checksum(codes) should be (12)
  }

  it("should properly count all doubles and triples") {
    PackageChecksum.countDoublesAndTriples("abcdef") should be (DoublesAndTriples(false, true))
    PackageChecksum.countDoublesAndTriples("bababc") should be (DoublesAndTriples(true, true))
    PackageChecksum.countDoublesAndTriples("abbcde") should be (DoublesAndTriples(true, false))
    PackageChecksum.countDoublesAndTriples("abcccd") should be (DoublesAndTriples(false, true))
    PackageChecksum.countDoublesAndTriples("aabcdd") should be (DoublesAndTriples(true, false))
    PackageChecksum.countDoublesAndTriples("abcdee") should be (DoublesAndTriples(true, false))
    PackageChecksum.countDoublesAndTriples("ababab") should be (DoublesAndTriples(false, true))
  }
}
