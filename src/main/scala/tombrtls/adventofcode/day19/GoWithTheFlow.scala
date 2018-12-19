package tombrtls.adventofcode.day19

import tombrtls.adventofcode.day16.Operation
import tombrtls.adventofcode.{Assignment, FileHelper}
import scala.math.Integral.Implicits._


case class Instruction(operation: Operation, a: Int, b: Int, c: Int) {
  override def toString: String =
    s"${operation} $a $b $c"
}

object GoWithTheFlow {
  val instructionPointerRegexg = """#ip (\d)""".r
  val instructionRegexp = """(.*) (\d*) (\d*) (\d*)""".r

  def divisor(input: Int) = {
    (1 to math.sqrt(input).ceil.toInt).iterator.flatMap({ value =>
      input /% value match {
        case (q, 0) => Iterator(value, q)
        case _ => Iterator.empty
      }
    }).sum
  }

  def main(args: Array[String]) = {
    val lines = FileHelper.readLines("/day19/input.txt")
    val instructionPointer = lines.head match {
      case instructionPointerRegexg(pointer) => pointer.toInt
    }

    val instructions = lines.tail.map {
      case instructionRegexp(ops, a, b, c) => {
        Instruction(Operation.fromString(ops), a.toInt, b.toInt, c.toInt)
      }
    }
    val stack = Array[Int](0, 0, 0, 0, 0, 0)
    var instructionIndex = 0
    while (instructionIndex < instructions.length) {
      stack.update(instructionPointer, instructionIndex)
      val instruction = instructions(instructionIndex)
      instruction.operation.execute(stack, instruction.a, instruction.b, instruction.c)
      instructionIndex = stack(instructionPointer) + 1
    }

    println(s"Stack: ${stack.mkString(", ")}")

    println(s"Assignment 2: ${divisor(10551373)}")
  }
}
