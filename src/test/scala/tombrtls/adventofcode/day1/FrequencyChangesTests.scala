package tombrtls.adventofcode.day1

import org.scalatest.{FunSpec, Matchers}
import tombrtls.adventofcode.day1.FrequencyChanges.accumulate

class FrequencyChangesTests extends FunSpec with Matchers {
  it("should properly find the final frequency") {
    accumulate(Seq(1, 1, 1)) should be (3)
    accumulate(Seq(1, 1, -2)) should be (0)
    accumulate(Seq(-1, -2, -3)) should be (-6)
  }
}
