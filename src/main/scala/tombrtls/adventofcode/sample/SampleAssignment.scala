package tombrtls.adventofcode.sample

import tombrtls.adventofcode.Assignment

object SampleAssignment extends Assignment[Seq[String], Int] {
  override val sampleInputFile: String = _
  override val sampleExpectation: Int = _
  override val inputFile: String = _

  override def processLines(lines: Seq[String]): Seq[String] = ???

  override def implementation(input: Seq[String]): Int = ???
}
