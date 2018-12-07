package tombrtls.adventofcode.day4

import java.time.{LocalDate, LocalDateTime}

import tombrtls.adventofcode.Assignment

object GuardDutyStrategy2 extends Assignment[Seq[Guard], Int] {
  def main(args: Array[String]): Unit = startAssignment

  override val day: Int = 4
  override val testCases = Seq(
    ("sample.txt", 4455)
  )

  override val inputFileName: String = "input.txt"

  private val entryRegex = "\\[(\\d*)-(\\d*)-(\\d*) (\\d*):(\\d*)\\] (.*)".r
  private val beginsShiftRegex = "Guard #(\\d*) begins shift".r
  private val fallsAsleepString = "falls asleep"
  private val wakesUpString = "wakes up"

  override def processLines(lines: Seq[String]): Seq[Guard] = {
    val activities = lines
      .map { line =>
        line match {
          case entryRegex(year, month, day, hour, minute, text) => {
            val localDateTime = LocalDateTime.of(year.toInt, month.toInt, day.toInt, hour.toInt, minute.toInt)

            text match {
              case beginsShiftRegex(number) => {
                BeginsShift(number.toInt, localDateTime)
              }
              case `fallsAsleepString` => FallsAsleep(localDateTime)
              case `wakesUpString` => WakesUp(localDateTime)
            }
          }
        }
      }
      .sortBy(_.dateTime)

      var guardId = 0
      val activityByGuardId = activities.foldLeft(Map[Int, Seq[Activity]]()) { (map, activity) =>
        activity match {
          case BeginsShift(id, _) => {
            guardId = id
            map
          }

          case asleep: FallsAsleep => {
            val activities = map.getOrElse(guardId, Seq())
            map.updated(guardId, activities :+ asleep)
          }

          case wakesUp: WakesUp => {
            val activities = map.getOrElse(guardId, Seq())
            map.updated(guardId, activities :+ wakesUp)
          }
        }
      }

      activityByGuardId.map { case (guardId, activities) =>
        val sleeps = activities
          .grouped(2)
          .foldLeft(Map[LocalDate, Seq[Int]]()) { (map, guardActivities) =>
            val fallsAsleep = guardActivities(0).asInstanceOf[FallsAsleep]
            val wakesUp = guardActivities(1).asInstanceOf[WakesUp]

            val localDate = fallsAsleep.dateTime.toLocalDate
            val currentSleep = map.getOrElse(localDate, Seq())

            val startMinute = fallsAsleep.dateTime.getMinute
            val endMinute = wakesUp.dateTime.getMinute
            val minutes = startMinute until endMinute
            map.updated(localDate, currentSleep ++: minutes)
          }

        Guard(guardId, sleeps)
      }
      .toList
  }

  implicit val localDateOrdering: Ordering[LocalDateTime] = _ compareTo _

  override def implementation(input: Seq[Guard]): Int = {
    val guard = input
        .sortBy { guard => guard.mostTimesAsleepInAMinute }
        .reverse
        .head

    guard.id * guard.minuteMostAsleep
  }
}
