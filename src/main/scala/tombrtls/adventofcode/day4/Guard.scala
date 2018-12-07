package tombrtls.adventofcode.day4

import java.time.LocalDate

case class Guard(id: Int, napsByDate: Map[LocalDate, Seq[Int]]) {
  val totalMinutesAsleep =
    napsByDate.values
      .flatten
      .reduce(_ + _)

  val timesAsleepPerMinute: Map[Int, Int] =
    napsByDate.values
      .foldLeft(Map[Int, Int]()) { (acc, range) =>
        range.foldLeft(acc) { (acc, minute) =>
          val count = acc.getOrElse(minute, 0)
          acc.updated(minute, count + 1)
        }
      }

  val minuteAndTimesMostAsleep: (Int, Int) =
    timesAsleepPerMinute.toSeq
      .sortBy(_._2)
      .reverse
      .head
}
