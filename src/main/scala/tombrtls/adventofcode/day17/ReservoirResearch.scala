package tombrtls.adventofcode.day17

import tombrtls.adventofcode.{FileHelper, Vector2}

import scala.annotation.tailrec

sealed abstract class Dimension
case object X extends Dimension
case object Y extends Dimension

object Dimension {
  def apply(char: Char) = char match {
    case 'x' => X
    case 'y' => Y
  }
}

case class ClayRange(point: Int, dimension: Dimension, range: Range) {
  def vectors: Seq[Vector2] =
    dimension match {
      case X => range.map(Vector2(_, point))
      case Y => range.map(Vector2(point, _))
    }
}

sealed abstract class Material {
  override def toString: String = this match {
    case Water => "~"
    case Clay => "#"
    case DrySand => "|"
    case Sand => "."
  }

  def isSolid: Boolean = this match {
    case Water => true
    case Clay => true
    case DrySand => false
    case Sand => false
  }
}
case object Water extends Material
case object Clay extends Material
case object Sand extends Material
case object DrySand extends Material

class UndergroundMap(clayRanges: Seq[ClayRange]) {
  def countAllTheWetStuff: Int = {
    map.flatMap { innerMap => innerMap}
      .count {
        case Water | DrySand => true
        case _ => false
      }
  }

  def countAllTheWater: Int = {
    map.flatMap { innerMap => innerMap }
      .count {
        case Water => true
        case _ => false
      }
  }

  val allVectors = clayRanges.flatMap(_.vectors)
  val minX = allVectors.minBy(_.x).x - 1
  val minY = allVectors.minBy(_.y).y - 1
  val maxX = allVectors.maxBy(_.x).x + 1
  val maxY = allVectors.maxBy(_.y).y + 1

  val map = new Array[Array[Material]](maxY - minY + 1)
  for (y <- minY to maxY) {
    val innerArray = new Array[Material](maxX - minX + 1)
    map.update(y - minY, innerArray)
    for (x <- minX to maxX) {
      if (allVectors.contains(Vector2(x, y))) {
        innerArray.update(x - minX, Clay)
      } else {
        innerArray.update(x - minX, Sand)
      }
    }
  }

  override def toString: String = {
    map
      .map(_.mkString(" "))
      .mkString("\r\n")
  }

  def materialAtLocation(vector: Vector2): Material = {
    map(vector.y - minY)(vector.x - minX)
  }

  def changeMaterialAtLocation(location: Vector2, material: Material) = {
    map(location.y - minY).update(location.x - minX, material)
  }

  def runWater(vector: Vector2): Unit = {
    runWaterDown(Vector2(vector.x, minY + 1))
  }

  /**
    * Returns true if it's a bowl
    * @param vector
    * @return
    */
  def runWaterSideways(vector: Vector2): Boolean = {

    @tailrec
    def runWaterSideways(vector: Vector2, direction: Vector2): (Boolean, Vector2) = {
      val nextVector = vector + direction
      val nextMaterial = materialAtLocation(nextVector)

      val nextBelowVector = nextVector + Vector2.Down
      val nextBelowMaterial = materialAtLocation(nextBelowVector)
      if (nextMaterial == Clay) {
        (true, nextVector)
      } else if (nextBelowMaterial.isSolid == false) {
        (false, nextVector)
      } else {
        runWaterSideways(nextVector, direction)
      }
    }

    val (hasLeftEdge, maxLeftLocation) = runWaterSideways(vector, Vector2.Left)
    val (hasRightEdge, maxRightLocation) = runWaterSideways(vector, Vector2.Right)

    if (hasLeftEdge && hasRightEdge) {
      for (x <- maxLeftLocation.x + 1 until maxRightLocation.x) {
        changeMaterialAtLocation(Vector2(x, vector.y), Water)
      }
      true
    } else {
      for (x <- maxLeftLocation.x + 1 until maxRightLocation.x) {
        changeMaterialAtLocation(Vector2(x, vector.y), DrySand)
      }
      if (hasLeftEdge == false) {
        runWaterDown(maxLeftLocation)
      }

      if (hasRightEdge == false){
        runWaterDown(maxRightLocation)
      }

      false
    }
  }

  @tailrec
  private def runWaterDown(vector: Vector2): Unit = {
    var nextVector = vector + Vector2.Down
    if (nextVector.y <= maxY) {
      materialAtLocation(nextVector) match {
        case material if (material.isSolid) => {
          if (runWaterSideways(vector)) {
            runWaterDown(nextVector + Vector2.Up + Vector2.Up)
          }
        }
        case Sand => {
          changeMaterialAtLocation(vector, DrySand)
          runWaterDown(nextVector)
        }
        case _ => // Do nothing
      }
    }
  }
}

object ReservoirResearch {
  def main(args: Array[String]): Unit = {
    val lines = FileHelper.readLines("/day17/input.txt")

    val regexp = "([xy])=(\\d*), ([xy])=(\\d*)..(\\d*)".r
    val ranges = lines.map {
      case regexp(_, point, xOrY2, startingPoint, endPoint) => {
        ClayRange(point.toInt, Dimension(xOrY2.head), startingPoint.toInt to endPoint.toInt)
      }
    }

    val undergroundMap = new UndergroundMap(ranges)
    println(s"${undergroundMap}")
    println("")
    println("")
    println("")
    println("")
    println("")
    println("")
    println("")
    println("")
    println("")
    println("")
    println("")

    undergroundMap.runWater(Vector2(500, 0))
    println(s"${undergroundMap}")

    println(s"${undergroundMap.countAllTheWetStuff}")
    println(s"${undergroundMap.countAllTheWater}")
  }
}
