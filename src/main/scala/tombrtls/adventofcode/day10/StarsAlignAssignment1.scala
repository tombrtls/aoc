package tombrtls.adventofcode.day10

import java.io.Writer

import javax.print.attribute.standard.MediaSize.Other
import tombrtls.adventofcode.{Assignment, FileHelper}

case class Vector2(x: Int, y: Int) {
  def plus(other: Vector2): Vector2 = {
    Vector2(x + other.x, y + other.y)
  }

  def minus(other: Vector2): Vector2 = {
    Vector2(x - other.x, y - other.y)
  }

  def mutliply(scaler: Int): Vector2 = {
    Vector2(x * scaler, y * scaler)
  }

  def + (other: Vector2): Vector2 = {
    this.plus(other)
  }

  def - (other: Vector2): Vector2 = {
    this.minus(other)
  }

  def * (scaler: Int): Vector2 = {
    this.mutliply(scaler)
  }
}

object Vector2 {
  val min = Vector2(Int.MinValue, Int.MinValue)
  val max = Vector2(Int.MaxValue, Int.MaxValue)
}

case class Bounds(x: Int, y: Int, width: Int, height: Int) {
  val horizontalRange = x to x + width
  val verticalRange = y to y + height
  val surface = width * height
  val distance = Math.abs(horizontalRange.end - horizontalRange.start) + Math.abs(verticalRange.end - verticalRange.start)
}

object Bounds {
  def apply(topLeft: Vector2, bottomRight: Vector2): Bounds = {
    val width = bottomRight.x - topLeft.x
    val height = bottomRight.y - topLeft.y
    Bounds(topLeft.x, topLeft.y, width, height)
  }
}

case class Star(location: Vector2, velocity: Vector2) {
  def after(seconds: Int): Star = {
    val newLocation = location + (velocity * seconds)
    Star(newLocation, velocity)
  }
}
object Star {
  def apply(x: String, y: String, velX: String, velY: String): Star =
    Star(Vector2(x.trim.toInt, y.trim.toInt), Vector2(velX.trim.toInt, velY.trim.toInt))
}

case class Sky(stars: Seq[Star]) {
  def after(seconds: Int): Sky = {
    Sky(stars.map(_.after(seconds)))
  }

  lazy val bounds: Bounds = {
    val minX = coordinates.map(_.x).min
    val maxX = coordinates.map(_.x).max
    val minY = coordinates.map(_.y).min
    val maxY = coordinates.map(_.y).max

    Bounds(Vector2(minX, minY), Vector2(maxX, maxY))
  }

  lazy val coordinates: Set[Vector2] =
    stars.map(_.location).toSet

  def writeToFile(writer: Writer): Unit = {
    for (y <- bounds.verticalRange) {
      for (x <- bounds.horizontalRange; vector = Vector2(x, y)) {
        coordinates.contains(vector) match {
          case true => writer.write("#")
          case false => writer.write(".")
        }
      }

      writer.write("\r\n")
    }
  }
}

object StarsAlignAssignment1 extends Assignment[Sky, Int] {
  def main(args: Array[String]): Unit = startAssignment

  override val day: Int = 10
  override val testCases: Seq[(String, Int)] = Seq(
    ("sample_0.txt", 3),
  )
  override val inputFileName: String = "input.txt"

  private val regexp = "position=<(.*), (.*)> velocity=<(.*), (.*)>".r
  override def processLines(lines: Seq[String]): Sky = {
    val stars = lines
      .map { case regexp(x, y, velX, velY) => Star(x, y, velX, velY) }
    Sky(stars)
  }


  override def implementation(input: Sky): Int = {
    val output = iterate(input, 0)

    FileHelper.fileWriter(s"output/output_${output._1}.txt", (writer) => {
      output._2.writeToFile(writer)
    })

    output._1
  }

  def iterate(sky: Sky, second: Int) : (Int, Sky) = {
    val nextSky = sky.after(1)
    if (nextSky.bounds.distance < sky.bounds.distance) {
      iterate(nextSky, second + 1)
    } else {
      (second, sky)
    }
  }
}
