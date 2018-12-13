package tombrtls.adventofcode.day8

import tombrtls.adventofcode.Assignment

case class Node(childNodes: Seq[Node], metadata: Seq[Int]) {
  def length: Int =
    childNodes
      .map(_.length)
      .foldLeft(0)(_ + _) + metadata.length

  def metadataCombined: Int =
    childNodes.foldLeft(0) { case (acc, node) => acc + node.metadataCombined } + metadata.foldLeft(0)(_ + _)
}

object Node {
  def apply(input: Seq[Int]): Node = {
    val (node, _) = createNodeAndRemainder(input)
    node
  }

  private def createNodeAndRemainder(input: Seq[Int]): (Node, Seq[Int]) = {
    val numberOfChildNodes = input(0)
    val numberOfMetaData = input(1)

    var remainder = input.drop(2)
    val childNodes = for (_ <- 0 until numberOfChildNodes) yield {
      val (node, newRemainder) = createNodeAndRemainder(remainder)
      remainder = newRemainder
      node
    }

    val metaData = remainder.take(numberOfMetaData)
    (Node(childNodes, metaData), remainder.drop(numberOfMetaData))
  }
}

object MemoryManeuverAssignment1 extends Assignment[Seq[Int], Int] {
  def main(args: Array[String]): Unit = startAssignment

  override val day: Int = 8
  override val testCases: Seq[(String, Int)] = Seq(
    ("sample.txt", 138)
  )

  override val inputFileName: String = "input.txt"

  override def processLines(lines: Seq[String]): Seq[Int] =
    lines.head.split(" ").map(_.toInt)

  override def implementation(input: Seq[Int]): Int = {
    Node(input).metadataCombined
  }
}
