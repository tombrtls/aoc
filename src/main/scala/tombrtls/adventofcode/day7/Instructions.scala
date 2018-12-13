package tombrtls.adventofcode.day7

case class Instructions(tasks: Seq[Task], completedTasks: Set[String] = Set()) {
  val availableTasks =
    tasks
      .filterNot { task => isCompleted(task.name) }
      .filter { task => task.dependencies.forall(isCompleted) }
      .map(_.name)
      .sorted


  def isCompleted(task: String): Boolean = completedTasks.contains(task)
  def completeTask(task: String): Instructions =
    Instructions(tasks, completedTasks ++ Seq(task))
}
