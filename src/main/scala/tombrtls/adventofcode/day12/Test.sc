import tombrtls.adventofcode.FileHelper

val INITIAL_STATE_REGEX = "initial state: ([.#]+)".r
val RULE_REGEX = "([.#]{5}) => ([.#])".r

val (initialState, rules) = {
  val instructions = FileHelper.readLines("/day12/input.txt").toVector

  (instructions.collect { case INITIAL_STATE_REGEX(state) => state }.ensuring(_.size == 1).head,
    instructions.collect { case RULE_REGEX(left, right) => left -> right.ensuring(_.size == 1).head }.toMap withDefaultValue '.')
}

implicit class RichString(val me: String) {
  def dropRightWhile(f: Char => Boolean): String = me take (me.lastIndexWhere(!f(_)) + 1)
}

val PADDING = "." * 4  // 2 to allow growth up to 2 tiles left/right, plus 2 to pad out the sliding window
case class SimState(offset: Int, plants: String) {
  def padded = copy(offset = offset + PADDING.size, plants = PADDING ++ plants ++ PADDING)
  def trimmed = copy(offset = offset - plants.indexOf('#'), plants = plants.dropWhile(_ == '.').dropRightWhile(_ == '.'))
}

val simStream = Stream.iterate(SimState(offset = 0, plants = initialState).trimmed) { state =>
  val newState = state.padded
  val newPlants = newState.plants.sliding(5).map(rules(_)).mkString
  newState.copy(offset = newState.offset - 2, plants = newPlants).trimmed
}

def printState(state: SimState) = {
  print(" " * (3 - state.offset))
  println(state.plants)
}

def sumPotIndices(state: SimState) = {
  state.plants.view.zipWithIndex.collect {
    case ('#', ind) => ind - state.offset
  }.sum
}

def part1 = {
  simStream take 21 foreach printState
  sumPotIndices(simStream(20))
}

def part2 = {
  // Scan for equilibrium (plants all the same, just "moving" along)
  val eqPoint = simStream.drop(1).zip(simStream).zipWithIndex.collect {
    case ((a, b), ind) if a.plants == b.plants => ind
  }.head

  println(eqPoint)

  // Fast forward to the right point in time
  val target = BigInt("50000000000")
  val offsetDiff = simStream(eqPoint + 1).offset - simStream(eqPoint).offset
  val timeDiff = target - eqPoint
  sumPotIndices(simStream(eqPoint)) - timeDiff * offsetDiff * simStream(eqPoint).plants.count(_ == '#')
}

println(part1)
println(part2)