package tombrtls.adventofcode.day4

import java.time.LocalDate

case class Guard(id: Int, minutesAsleepByDate: Map[LocalDate, Seq[Int]]) {
  val totalMinutesAsleep =
    minutesAsleepByDate.values
      .flatten
      .reduce(_ + _)

  val timesAsleepPerMinute: Map[Int, Int] =
    minutesAsleepByDate.values
      .foldLeft(Map[Int, Int]()) { (acc, range) =>
        range.foldLeft(acc) { (acc, minute) =>
          val count = acc.getOrElse(minute, 0)
          acc.updated(minute, count + 1)
        }
      }

  private val minuteTimeMostAsleep: (Int, Int) =
    timesAsleepPerMinute.toSeq
      .sortBy(_._2)
      .reverse
      .head

  val minuteMostAsleep = minuteTimeMostAsleep._1
  val mostTimesAsleepInAMinute = minuteTimeMostAsleep._2
}
