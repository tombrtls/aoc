package tombrtls.adventofcode.day1

import org.scalatest.{FunSpec, Matchers}
import tombrtls.adventofcode.day1.FrequencyDuplicateDetection.detectDuplicateFreq

class FrequencyDuplicateDetectionTests extends FunSpec with Matchers {
  it("should properly detect the first duplicate frequency when going through a list of deltas") {
    detectDuplicateFreq(Seq(1, -1)) should be (0)
    detectDuplicateFreq(Seq(3, 3, 4, -2, -4)) should be (10)
    detectDuplicateFreq(Seq(-6, 3, 8, 5, -6)) should be (5)
    detectDuplicateFreq(Seq(7, 7, -2 , -7, -4)) should be (14)
  }
}
