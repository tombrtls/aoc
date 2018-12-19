package tombrtls.adventofcode

import tombrtls.adventofcode.day18.Acre

import scala.reflect.ClassTag

case class Grid[A](width: Int, height: Int, space: Array[Array[A]]) extends Cloneable {
  def updateItemAt(location: Vector2, item: A): Unit =
    space(location.y).update(location.x, item)

  def itemAt(location: Vector2): A =
    space(location.y)(location.x)

  def isLocationInGrid(location: Vector2): Boolean =
    location.x >= 0 && location.y >= 0 && location.x < width && location.y < height

  def allItems: Seq[A] = space.flatMap { line => line }

  def process(func: (Vector2, A) => A)(implicit classTag: ClassTag[A]): Grid[A] = {
    val newSpace = new Array[Array[A]](height)
    for ((line, y) <- space.zipWithIndex) {
      val innerArray = new Array[A](width)
      newSpace.update(y, innerArray)
      for ((item, x) <- line.zipWithIndex) {
        val newItem = func(Vector2(x, y), item)
        innerArray.update(x, newItem)
      }
    }
    Grid(width, height, newSpace)
  }

  override def toString: String =
    space.map(_.mkString(" ")).mkString("\r\n")
}

object Grid {
  def from[A](lines: Seq[String], processor: (Char) => A)(implicit classTag: ClassTag[A]): Grid[A] = {
    val maxY = lines.length - 1
    val maxX = lines.head.length - 1
    val map = new Array[Array[A]](maxY + 1)
    for ((line, y) <- lines.zipWithIndex) {
      val innerMap = new Array[A](maxX + 1)
      map.update(y, innerMap)

      for ((char, x) <- line.zipWithIndex) {
        innerMap.update(x, processor(char))
      }
    }

    Grid(maxX + 1, maxY + 1, map)
  }
}