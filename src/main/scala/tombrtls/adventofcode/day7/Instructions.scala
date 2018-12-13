package tombrtls.adventofcode.day7

case class Instructions(tasks: Seq[Task], completedTasks: Set[Char] = Set()) {
  val availableTasks =
    tasks
      .filterNot { task => isCompleted(task.name) }
      .filter { task => task.dependencies.forall(isCompleted) }
      .map(_.name)
      .sorted


  def isCompleted(task: Char): Boolean = completedTasks.contains(task)
  def completeTask(task: Char): Instructions =
    Instructions(tasks, completedTasks ++ Seq(task))
}
