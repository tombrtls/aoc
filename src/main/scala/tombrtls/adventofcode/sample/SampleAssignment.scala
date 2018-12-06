package tombrtls.adventofcode.sample

import tombrtls.adventofcode.Assignment

object SampleAssignment {
  def main(args: Array[String]): Unit = {
    verify
    run
  }

  private val verificationInput = Seq("")
  private val verificationExpectedOutput = 0
  private def verify = {
    val output = sampleImplementation(verificationInput)
    if (output == verificationExpectedOutput) {
      println("Verification")
      println("========================")
      println(s"Success ✔️: '${output}' is equal to '${verificationExpectedOutput}'\r\n")
    } else {
      println(s"failure 🛑: '${output}' is NOT equal to '${verificationExpectedOutput}'\r\n")
      println("")
    }
  }

  private val input = SampleData.input
  private def run = {
    val output = sampleImplementation(input)
    println("Run")
    println("========================")
    println(s"Output: ${output}")
    println("")
  }

  def sampleImplementation(input: Seq[String]): Int = 0
}
