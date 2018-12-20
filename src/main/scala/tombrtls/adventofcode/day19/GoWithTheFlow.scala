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
    var stack = Array[Int](1, 0, 0, 0, 0, 0)
    var instructionIndex = 0
    while (instructionIndex < instructions.length) {
      stack.update(instructionPointer, instructionIndex)
      if (instructionIndex == 3) {
        stack = step3(stack)
      } else {
        val instruction = instructions(instructionIndex)
        instruction.operation.execute(stack, instruction.a, instruction.b, instruction.c)
      }

      if (instructionIndex == 3 && (stack(5) < 10 || stack(5) > 10551354 - 10)) {
        println(s"[${stack.mkString(", ")}]")
      }

      instructionIndex = stack(instructionPointer) + 1

    }

    println(s"Stack: ${stack.mkString(", ")}")

    println(s"Assignment 2: ${divisor(10551373)}")
  }


  // Simplify step 3
  // mulr 4 5 1
  // eqrr 1 2 1
  // addr 1 3 3
  // addi 3 1 3
  // addi 5 1 5
  // gtrr 5 2 1
  // addr 3 1 3
  // seti 2 6 3

  // Pointer = 3
  def step3(stack: Array[Int]): Array[Int] = {
    var Array(reg0, reg1, reg2, reg3, reg4, reg5) = stack
    reg1 = reg4 * reg5
    reg3 += 1

    reg1 = if (reg1 == reg2) 1 else 0
    reg3 += 1

    reg3 = reg1 + reg3
    reg3 += 1

    reg3 = reg3 + 1
    reg3 += 1

    reg5 = reg5 + 1
    reg3 += 1

    reg1 = if (reg1 > reg2) 1 else 0
    reg3 += 1

    reg3 = reg3 + reg1
    reg3 += 1

    reg3 = 2
    Array(reg0, reg1, reg2, reg3, reg4, reg5)
  }
}
