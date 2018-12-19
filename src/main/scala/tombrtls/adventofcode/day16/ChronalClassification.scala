package tombrtls.adventofcode.day16

import tombrtls.adventofcode.FileHelper

object ChronalClassification {

  val registerRegexp = """.*\[(\d*), (\d*), (\d*), (\d*)\]""".r
  val instructionsRegexp = """(\d*) (\d*) (\d*) (\d*)""".r

  def main(args: Array[String]): Unit = {
    val samples = readSamples("/day16/input.txt")
    val sampleToOperations = executeOperationsOnSamples(samples, Operation.allOperations.toSet)
    val opcodeToOperation = determineOpcodeToOperation(sampleToOperations)

    val instructions = FileHelper.readLines("/day16/input2.txt")
        .map {
          case instructionsRegexp(first, second, third, fourth) => Seq(first, second, third, fourth).map(_.toInt)
        }

    val stack = instructions.foldLeft(List(0, 0, 0, 0)) { (stack, instruction) =>
      val Seq(opcode, a, b, c) = instruction
      val operation = opcodeToOperation(opcode)
      operation.execute(stack, a, b, c)
    }

    println(s"${stack(0)}")
  }

  def readSamples(fileName: String) = {
    val lines = FileHelper.readLines("/day16/input.txt")
    lines.filterNot(_.length == 0).sliding(3, 3)
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

        val Seq(a, b, c) = instructions.tail
        OperationSample(instructions.head, before.toList, a, b, c, after.toList)
      }
      .toList
  }

  def executeOperationsOnSamples(samples: Seq[OperationSample], operations: Set[Operation]): Map[OperationSample, Set[Operation]] = {
    var sampleToOperations: Map[OperationSample, Set[Operation]] = Map.empty.withDefaultValue(Set.empty)
    for (sample <- samples; operation <- Operation.allOperations) {
      operation.execute(sample.before, sample.a, sample.b, sample.c) == sample.after match {
        case true => {
          val operations = sampleToOperations(sample)
          sampleToOperations = sampleToOperations.updated(sample, operations + operation)
        }
        case false => {
          // Do Nothing
        }
      }
    }
    sampleToOperations
  }

  def determineOpcodeToOperation(sampleToOperations: Map[OperationSample, Set[Operation]]): Map[Int, Operation] = {
    val opcodeToOperations = sampleToOperations
      .groupBy(_._1.opsCode)
      .mapValues { _.values }
      .mapValues { _.reduce(_.intersect(_)) }

    (0 until Operation.allOperations.size).foldLeft(Map[Int, Operation]()) { (map, _) =>
      val singleOperations = opcodeToOperations
        .mapValues(_ -- map.values)
        .filter(_._2.size == 1)

      map ++ singleOperations.mapValues(_.head)
    }
  }

  def tests = {
    def test(operation: Operation, opsInput: List[Int], inputStack: List[Int], expected: Seq[Int]) = {
      val Seq(a, b, c) = opsInput
      operation.execute(inputStack, a, b, c) == expected match {
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
    test(GreaterThanRI, List(1, 5, 3), stack, List(4, 3, 2, 0))

    test(GreaterThanRR, opsInput, stack, List(4, 3, 2, 1))
    test(GreaterThanRR, List(1, 0, 3), stack, List(4, 3, 2, 0))

    test(EqualIR, opsInput, stack, List(4, 3, 2, 0))
    test(EqualIR, List(4, 0, 3), stack, List(4, 3, 2, 1))

    test(EqualRI, opsInput, stack, List(4, 3, 2, 0))
    test(EqualRI, List(0, 4, 3), stack, List(4, 3, 2, 1))

    test(EqualRR, opsInput, stack, List(4, 3, 2, 0))
    test(EqualRR, List(0, 0, 3), stack, List(4, 3, 2, 1))
  }
}
