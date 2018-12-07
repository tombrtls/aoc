package tombrtls.adventofcode.day1

import tombrtls.adventofcode.Assignment

import scala.annotation.tailrec

object FrequencyDuplicateDetection extends Assignment[Seq[Int], Int] {
  def main(args: Array[String]): Unit = startAssignment

  override val day: Int = 1
  override val testCases = Seq(
    ("duplication_sample_1.txt", 0),
    ("duplication_sample_2.txt", 10),
    ("duplication_sample_3.txt", 5),
    ("duplication_sample_4.txt", 14)
  )
  override val inputFileName: String = "input.txt"

  override def processLines(lines: Seq[String]): Seq[Int] = lines.map(_.toInt)

  override def implementation(input: Seq[Int]): Int = {
    @tailrec
    def detectDuplicateFreqInner(currentFrequency: Int, remainingDeltas: Stream[Int], knownFrequencies: Set[Int]): Int = {
      if (knownFrequencies.contains(currentFrequency)) {
        currentFrequency
      } else {
        detectDuplicateFreqInner(
          currentFrequency + remainingDeltas.head,
          remainingDeltas.tail,
          knownFrequencies + currentFrequency
        )
      }
    }

    val inputStream = Stream.continually(input.toStream).flatten
    detectDuplicateFreqInner(0, inputStream, Set())
  }
}
