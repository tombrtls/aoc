package tombrtls.adventofcode.day2

import org.scalatest.{FunSpec, Matchers}

class BoxIdFinderTests extends FunSpec with Matchers {
  it("should properly find the similar boxIds") {
    val boxIds = Seq(
      "abcde",
      "fghij",
      "klmno",
      "pqrst",
      "fguij",
      "axcye",
      "wvxyz"
    )
    BoxIdFinder.findSimilarBoxIds(boxIds) should be ("fghij", "fguij")
    BoxIdFinder.common("fghij", "fguij") should be ("fgij")
  }

  it("should be able to tell when two strings have 1 character in difference") {
    BoxIdFinder.singleCharacterDifferent("abcdefg")("abcdefe") should be (true)
    BoxIdFinder.singleCharacterDifferent("abcdefg")("abedefg") should be (true)
    BoxIdFinder.singleCharacterDifferent("abcdefg")("abadedg") should be (false)
    BoxIdFinder.singleCharacterDifferent("abcdefg")("gfedcba") should be (false)
  }

  it("should be able to tell the common parts of the string") {
    BoxIdFinder.common("abc", "abd") should be ("ab")
    BoxIdFinder.common("abc", "dbc") should be ("bc")
  }
}
