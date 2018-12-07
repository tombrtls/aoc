package tombrtls.adventofcode.day4

import java.time.LocalDateTime

trait Activity {
  val dateTime: LocalDateTime
}

case class BeginsShift(id: Int, dateTime: LocalDateTime) extends Activity
case class FallsAsleep(dateTime: LocalDateTime) extends Activity
case class WakesUp(dateTime: LocalDateTime) extends Activity
