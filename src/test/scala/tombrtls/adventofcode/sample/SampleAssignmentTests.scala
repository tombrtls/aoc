package tombrtls.adventofcode.sample

import org.scalatest.{FunSpec, Matchers}

class SampleAssignmentTests extends FunSpec with Matchers {
  it("should properly convert stuff") {
    val input = Seq("")
    SampleAssignment.sampleImplementation(input) should be (1)
  }
}
