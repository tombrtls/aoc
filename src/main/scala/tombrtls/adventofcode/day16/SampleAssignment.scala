package tombrtls.adventofcode.day16

import tombrtls.adventofcode.FileHelper

case class Sample(opsCode: Int, before: List[Int], operation: List[Int], after: List[Int])

sealed abstract class Operation {
  def execute(stack: List[Int], input: List[Int]): List[Int] = {
    val Seq(a, b, output) = input
    val value = this match {
      case AddR => stack(a) + stack(b)
      case AddI => stack(a) + b
      case MultiR => stack(a) * stack(b)
      case MultiI => stack(a) * b
      case BanR => stack(a) & stack(b)
      case BanI => stack(a) & b
      case BorR => stack(a) | stack(b)
      case BorI => stack(a) | b
      case SetR => stack(a)
      case SetI => a
      case GreaterThanIR => if (a > stack(b)) 1 else 0
      case GreaterThanRI => if (stack(a) > b) 1 else 0
      case GreaterThanRR => if (stack(a) > stack(b)) 1 else 0
      case EqualIR => if (a == stack(b)) 1 else 0
      case EqualRI => if (stack(a) == b) 1 else 0
      case EqualRR => if (stack(a) == stack(b)) 1 else 0
    }

    stack.updated(output, value)
  }
}

object Operation {
  val allOperations = Set(
    AddR, AddI,
    MultiI, MultiR,
    BanR, BanI,
    BorR, BorI,
    SetR, SetI,
    GreaterThanIR, GreaterThanRI, GreaterThanRR,
    EqualIR, EqualRI, EqualRR)
}

case object AddR extends Operation
case object AddI extends Operation
case object MultiR extends Operation
case object MultiI extends Operation
case object BanR extends Operation
case object BanI extends Operation
case object BorR extends Operation
case object BorI extends Operation
case object SetR extends Operation
case object SetI extends Operation
case object GreaterThanIR extends Operation
case object GreaterThanRI extends Operation
case object GreaterThanRR extends Operation
case object EqualIR extends Operation
case object EqualRI extends Operation
case object EqualRR extends Operation


object SampleAssignment {

  val registerRegexp = """.*\[(\d*), (\d*), (\d*), (\d*)\]""".r
  val instructionsRegexp = """(\d*) (\d*) (\d*) (\d*)""".r

  def main(args: Array[String]): Unit = {

    val lines = FileHelper.readLines("/day16/input.txt")
    val samples = lines.filterNot(_.length == 0).sliding(3, 3)
      .map { sampleInput =>
        val before = sampleInput.head match {
          case registerRegexp(first, second, third, fourth) => Seq(first, second, third, fourth).map(_.toInt)
        }

        val instructions = sampleInput(1) match {
          case instructionsRegexp(first, second, third, fourth) => Seq(first, second, third, fourth).map(_.toInt)
        }

        val after = sampleInput.last match {
          case registerRegexp(first, second, third, fourth) => Seq(first, second, third, fourth).map(_.toInt)
        }

        Sample(instructions.head, before.toList, instructions.tail.toList, after.toList)
      }

    var opsCodeToOperations: Map[Int, Set[Operation]] = Map.empty.withDefaultValue(Set.empty)
    for (sample <- samples; operation <- Operation.allOperations) {
      operation.execute(sample.before, sample.operation) == sample.after match {
        case true => {
          val operations = opsCodeToOperations(sample.opsCode)
          opsCodeToOperations = opsCodeToOperations.updated(sample.opsCode, operations + operation)
        }
        case false => {
          // Do Nothing
        }
      }
    }

    def test(operation: Operation, opsInput: List[Int], inputStack: List[Int], expected: Seq[Int]) = {
      operation.execute(inputStack, opsInput) == expected match {
        case true => println(s"${operation} matched expectation")
        case false => println(s"${operation} did not match expectation")
      }
    }

    val opsInput = List(0, 1, 3)
    val stack = List(4, 3, 2, Int.MinValue)
    test(AddR, opsInput, stack, List(4, 3, 2, 7))
    test(AddI, opsInput, stack, List(4, 3, 2, 5))
    test(MultiR, opsInput, stack, List(4, 3, 2, 12))
    test(MultiI, opsInput, stack, List(4, 3, 2, 4))
    test(BanR, opsInput, stack, List(4, 3, 2, 0))
    test(BanI, opsInput, stack, List(4, 3, 2, 0))
    test(BorR, opsInput, stack, List(4, 3, 2, 7))
    test(BorI, opsInput, stack, List(4, 3, 2, 5))
    test(GreaterThanIR, opsInput, stack, List(4, 3, 2, 0))
    test(GreaterThanIR, List(5, 0, 3), stack, List(4, 3, 2, 1))

    test(GreaterThanRI, opsInput, stack, List(4, 3, 2, 1))
    test(GreaterThanIR, List(5, 0, 3), stack, List(4, 3, 2, 1))


    test(GreaterThanRR, opsInput, stack, List(4, 3, 2, 1))
    test(EqualIR, opsInput, stack, List(4, 3, 2, 0))
    test(EqualRI, opsInput, stack, List(4, 3, 2, 0))
    test(EqualRR, opsInput, stack, List(4, 3, 2, 0))



    println(s"${opsCodeToOperations}")
    println("")

    val count = opsCodeToOperations.count { case (opsCode, operations) => operations.size >= 3 }
    println(s"Count: ${count}")
  }
}
