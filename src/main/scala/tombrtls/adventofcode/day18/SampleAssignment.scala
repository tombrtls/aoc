package tombrtls.adventofcode.day18

import tombrtls.adventofcode.{FileHelper, Vector2}

sealed abstract class Acre {
  override def toString: String = this match {
    case Ground => "."
    case Trees => "|"
    case Lumberyard => "#"
  }
}

object Acre {
  def fromChar(char: Char): Acre = char match {
    case '.' => Ground
    case '|' => Trees
    case '#' => Lumberyard
  }
}

final case object Ground extends Acre
final case object Trees extends Acre
final case object Lumberyard extends Acre

case class Area(maxX: Int, maxY: Int, map: Array[Array[Acre]]) {
  def acreAt(location: Vector2): Acre = {
    map(location.y)(location.x)
  }

  def progress: Area = {
    val newMap = new Array[Array[Acre]](maxY + 1)
    for (y <- 0 to maxY) {
      val newInnerMap = new Array[Acre](maxX + 1)
      newMap.update(y, newInnerMap)
      for (x <- 0 to maxX; location = Vector2(x, y)) {
        val acres = surroundingAcre(location)
        val newAcre = acreAt(location) match {
          case Ground if (acres.count(_ == Trees) >= 3) => Trees
          case Trees if (acres.count(_ == Lumberyard) >= 3) => Lumberyard
          case Lumberyard if (acres.count(_ == Lumberyard) < 1 || acres.count(_ == Trees) < 1) => Ground
          case acre => acre
        }
        newInnerMap.update(x, newAcre)
      }
    }

    Area(maxX, maxY, newMap)
  }

  def vectorInRange(vector: Vector2): Boolean = {
    vector.x >= 0 && vector.y >= 0 && vector.x <= maxX && vector.y <= maxY
  }

  def surroundingAcre(vector: Vector2): List[Acre] = {
    Vector2.SurroundingVectors.map(_ + vector)
      .filter(vectorInRange)
      .toList
      .map(acreAt)
  }

  def score = {
    val allAcres = map.flatMap { acres => acres }
    val numberOfWood = allAcres.count(_ == Trees)
    val numberOfLumberYards = allAcres.count(_ == Lumberyard)
    numberOfWood * numberOfLumberYards
  }

  override def toString: String = {
    map.map(_.mkString(" "))
      .mkString("\r\n")
  }
}

object SampleAssignment  {

  def main(args: Array[String]): Unit = {

    val lines = FileHelper.readLines("/day18/input.txt")
    val maxY = lines.length - 1
    val maxX = lines.head.length - 1
    val map = new Array[Array[Acre]](maxY + 1)
    for ((line, y) <- lines.zipWithIndex) {
      val innerMap = new Array[Acre](maxX + 1)
      map.update(y, innerMap)

      for ((char, x) <- line.zipWithIndex) {
        innerMap.update(x, Acre.fromChar(char))
      }
    }

    def assignment1: Unit = {
      val area = (0 until 10).foldLeft(Area(maxX, maxY, map)) { (area, _) => area.progress }
      println("Assignment 1")
      println(s"Score: ${area.score}")
    }

    def assignment2: Unit = {
      def findIndicesForSameScore(area: Area, scoresToIndices: Map[Long, List[Long]], index: Long): (Area, List[Long]) = {
        val nextArea = area.progress
        val nextScore = nextArea.score
        scoresToIndices.get(nextScore) match {
          case None =>
            findIndicesForSameScore(nextArea, scoresToIndices.updated(nextScore, List(0)), index + 1)

          case Some(list) if (list.length < 3) =>
            findIndicesForSameScore(nextArea, scoresToIndices.updated(nextScore, list :+ index), index + 1)

          case Some(list) =>
            (nextArea, list :+ index)
        }
      }

      val area = Area(maxX, maxY, map)
      val (newArea, indices) = findIndicesForSameScore(area, Map().withDefaultValue(List.empty), 1)
      val Seq(last, secondLast) = indices.reverse.take(2)
      val indexDelta = last - secondLast
      val remainingIncrements = Math.floor((1000000000l - last) / indexDelta).toLong
      val nextIndex = last + (indexDelta * remainingIncrements)
      val endArea = (nextIndex until 1000000000l).foldLeft(newArea) { (area, index) =>
        area.progress
      }

      println("Assignment 2")
      println(s"Score: ${endArea.score}")
    }

    assignment1

    println("")

    assignment2
  }
}
