package tombrtls.adventofcode.day16

sealed abstract class Operation {

  def execute(stack: Array[Int], a: Int, b: Int, c: Int): Unit = {
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

    stack.update(c, value)
  }

  def execute(stack: List[Int], a: Int, b: Int, c: Int): List[Int] = {
    val array = stack.toArray
    execute(array, a, b, c)
    array.toList
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

  def fromString(string: String): Operation = string match {
    case "addr" => AddR
    case "addi" => AddI
    case "mulr" => MultiR
    case "muli" => MultiI
    case "banr" => BanR
    case "bani" => BanI
    case "borr" => BorR
    case "bori" => BorI
    case "setr" => SetR
    case "seti" => SetI
    case "gtir" => GreaterThanIR
    case "gtri" => GreaterThanRI
    case "gtrr" => GreaterThanRR
    case "eqir" => EqualIR
    case "eqri" => EqualRI
    case "eqrr" => EqualRR


  }
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