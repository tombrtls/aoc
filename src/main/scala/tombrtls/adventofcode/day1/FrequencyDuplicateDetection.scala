package tombrtls.adventofcode.day1

import scala.annotation.tailrec

object FrequencyDuplicateDetection {
  def main(args: Array[String]): Unit = {
    print(s"Output: ${detectDuplicateFreq(FrequencyData.frequencyChanges)}")
  }

  def detectDuplicateFreq(input: Seq[Int]): Int = {
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
