package tombrtls.adventofcode.day18

import tombrtls.adventofcode.{FileHelper, Grid, Vector2}

import scala.annotation.tailrec

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

case class Area(grid: Grid[Acre]) {
  def progress: Area = {
    val newGrid = grid.process { (location, value) =>
      val acres = location.surrounding.filter(grid.isLocationInGrid).map(grid.itemAt(_))
      value match {
        case Ground if (acres.count(_ == Trees) >= 3) => Trees
        case Trees if (acres.count(_ == Lumberyard) >= 3) => Lumberyard
        case Lumberyard if (acres.count(_ == Lumberyard) < 1 || acres.count(_ == Trees) < 1) => Ground
        case acre => acre
      }
    }

    Area(newGrid)
  }

  def score = {
    val allAcres = grid.allItems
    val numberOfWood = allAcres.count(_ == Trees)
    val numberOfLumberYards = allAcres.count(_ == Lumberyard)
    numberOfWood * numberOfLumberYards
  }

  override def toString: String = grid.toString
}

object SetllersOfTheNorthPole  {

  def main(args: Array[String]): Unit = {

    val lines = FileHelper.readLines("/day18/input.txt")
    val grid: Grid[Acre] = Grid.from(lines, Acre.fromChar)


    def assignment1: Unit = {
      val initialArea = Area(grid)
      println(s"Initial state: \r\n${initialArea}\r\n")
      val area = (0 until 10).foldLeft(initialArea) { (area, index) =>
        val newArea = area.progress
        println(s"Area after ${index + 1} minute")
        println(s"${newArea}")
        println("")
        newArea
      }
      println("Assignment 1")
      println(s"Score: ${area.score}")
    }

    def assignment2: Unit = {
      @tailrec
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

      val area = Area(grid)
      val (newArea, indices) = findIndicesForSameScore(area, Map().withDefaultValue(List.empty), 1)
      val Seq(last, secondLast) = indices.reverse.take(2)
      val indexDelta = last - secondLast
      val remainingIncrements = Math.floor((1000000000l - last) / indexDelta).toLong
      val nextIndex = last + (indexDelta * remainingIncrements)
      val endArea = (nextIndex until 1000000000l).foldLeft(newArea) { (area, _) =>
        area.progress
      }

      println("Assignment 2")
      println(s"Score: ${endArea.score}")
    }

    assignment1

    println("")

//    assignment2
  }
}
