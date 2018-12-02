package tombrtls.adventofcode.day1


object FrequencyChanges {
  def main(args: Array[String]): Unit = {
    print(s"Output: ${accumulate(FrequencyData.frequencyChanges)}")
  }

  def accumulate(input: Seq[Int]): Int =
    input.reduce(_ + _)
}
