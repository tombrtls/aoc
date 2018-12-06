package tombrtls.adventofcode

abstract class Assignment[Input, Output] {

  val sampleInputFile: String
  val sampleExpectation: Output

  val inputFile: String

  def processLines(lines: Seq[String]): Input

  def implementation(input: Input): Output

  def verify() = {
    val sampleInput = inputFromFile(sampleInputFile)
    val output = implementation(sampleInput)

    println("Verification")
    println("----------------------------")
    if (output == sampleExpectation) {
      println(s"Success ✔️: '${output}' is equal to '${sampleExpectation}'")
    } else {
      println(s"failure 🛑: '${output}' is NOT equal to '${sampleExpectation}'")
    }
    println("----------------------------")
    println("")
  }

  def run() = {
    val input = inputFromFile(inputFile)
    val output = implementation(input)
    println("Run")
    println("----------------------------")
    println(s"Output: ${output}")
    println("----------------------------")
    println("")
  }

  private def inputFromFile(file: String): Input =
    processLines(FileHelper.readLines(file))
}
