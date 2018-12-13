package tombrtls.adventofcode.day7

import tombrtls.adventofcode.Assignment


object SumOfPartsAssignment2 extends Assignment[TimeInstructions, Int] {
  def main(args: Array[String]): Unit = startAssignment

  override val day: Int = 7
  override val testCases: Seq[(String, Int)] = Seq(
    ("sample.txt", 15)
  )

  override val inputFileName: String = "input.txt"

  val regexp = "Step ([a-zA-Z]*) must be finished before step ([a-zA-Z]*) can begin.".r
  override def processLines(lines: Seq[String]): TimeInstructions = {
    val tasksAndDependency = lines
      .map { case regexp(dependency, task) => (task.head, dependency.head) }

    val tasksToDependencies = tasksAndDependency
      .flatMap { tasksAndDependency => Seq(tasksAndDependency._1, tasksAndDependency._2) }
      .toSet
      .foldLeft(Map[Char, Set[Char]]()) { (map, task) => map.updated(task, Set()) }

    val tasks = tasksAndDependency
      .foldLeft(tasksToDependencies) { (acc, taskAndDependency) =>
        val dependencies = acc.getOrElse(taskAndDependency._1, Set())
        val newDependencies = dependencies ++ Set(taskAndDependency._2)
        acc.updated(taskAndDependency._1, newDependencies)
      }
      .map { case (task, dependencies) => Task(task, dependencies)}

    TimeInstructions(tasks.toSeq, 0, Map(), 60)
  }

  override def implementation(input: TimeInstructions): Int = {
    completeNextTask(input)
  }

  def completeNextTask(input: TimeInstructions): Int = {
    val startedInstructions = input.startMaximumNumberOfTasks(5)

    if (startedInstructions.active) {
      completeNextTask(startedInstructions.proceedSecond)
    } else {
      startedInstructions.time
    }
  }
}
