package tombrtls.adventofcode.day8

case class Node(childNodes: Seq[Node], metadata: Seq[Int]) {
  def length: Int =
    childNodes
      .map(_.length)
      .foldLeft(0)(_ + _) + metadata.length

  def metadataCombined: Int =
    childNodes.foldLeft(0) { case (acc, node) => acc + node.metadataCombined } + metadata.foldLeft(0)(_ + _)

  lazy val value: Int =
    childNodes match {
      case Nil => metadata.foldLeft(0)(_ + _)
      case nodes => {
        val values = for (
          number <- metadata;
          node <- nodes.lift(number - 1)
        ) yield node.value

        values
          .foldLeft(0)(_ + _)
      }
    }
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