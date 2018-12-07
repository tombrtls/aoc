package tombrtls.adventofcode

abstract class Assignment[Input, Output] {

  type TestCase = (String, Output)

  val day: Int
  val testCases: Seq[TestCase]

  val inputFileName: String

  def processLines(lines: Seq[String]): Input

  def implementation(input: Input): Output

  def startAssignment(): Unit = {
    verify
    run
  }

  private def verify() = {
    println("Verification")
    println("----------------------------")

    testCases.foreach { case (fileName, expectedOutput) =>
      val sampleInput = inputFromFile(fileName)
      val output = implementation(sampleInput)
      println(s"Input: ${fileName}")
      if (output == expectedOutput) {
        println(s"Success ✔️: '${output}' is equal to '${expectedOutput}'")
      } else {
        println(s"failure 🛑: '${output}' is NOT equal to '${expectedOutput}'")
      }
      println("")
    }
    println("----------------------------")
    println("")
    println("")
  }

  private def run() = {
    val input = inputFromFile(inputFileName)
    val output = implementation(input)
    println("Run")
    println("----------------------------")
    println(s"Output: ${output}")
    println("----------------------------")
    println("")
  }

  private def inputFromFile(file: String): Input = {
    val dir = s"/day$day/$file"
    processLines(FileHelper.readLines(dir))
  }
}
