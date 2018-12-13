package tombrtls.adventofcode.day7

import tombrtls.adventofcode.Assignment


object SumOfPartsAssignment1 extends Assignment[Instructions, String] {
  def main(args: Array[String]): Unit = startAssignment

  override val day: Int = 7
  override val testCases: Seq[(String, String)] = Seq(
    ("sample.txt", "CABDFE")
  )

  override val inputFileName: String = "input.txt"

  val regexp = "Step ([a-zA-Z]*) must be finished before step ([a-zA-Z]*) can begin.".r
  override def processLines(lines: Seq[String]): Instructions = {
    val tasksAndDependency = lines
      .map { case regexp(dependency, task) => (task, dependency) }

    val tasksToDependencies = tasksAndDependency
      .flatMap { tasksAndDependency => Seq(tasksAndDependency._1, tasksAndDependency._2) }
      .toSet
      .foldLeft(Map[String, Set[String]]()) { (map, task) => map.updated(task.toString, Set()) }

    val tasks = tasksAndDependency
      .foldLeft(tasksToDependencies) { (acc, taskAndDependency) =>
        val dependencies = acc.getOrElse(taskAndDependency._1, Set())
        val newDependencies = dependencies ++ Set(taskAndDependency._2)
        acc.updated(taskAndDependency._1, newDependencies)
      }
      .map { case (task, dependencies) => Task(task, dependencies)}

    Instructions(tasks.toSeq, Set())
  }

  override def implementation(input: Instructions): String = {
    completeNextTask(input, "")
  }

  def completeNextTask(input: Instructions, output: String): String = {
    input.availableTasks match {
      case x +: _ => completeNextTask(input.completeTask(x), output + x)
      case Nil => output
    }
  }
}
