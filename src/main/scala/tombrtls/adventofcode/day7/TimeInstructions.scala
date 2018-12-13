package tombrtls.adventofcode.day7

case class TimeInstructions(tasks: Seq[Task], time: Int, startedTasks: Map[Char, Int], defaultDuration: Int = 60) {
  val completedTasks: Set[Char] =
    startedTasks
      .filterKeys(isCompleted)
      .keySet

  val runningTasks: Set[Char] =
    startedTasks
      .filterKeys(isRunning)
      .keySet

  val availableTasks =
    tasks
      .filterNot { task => isCompleted(task.name) || isRunning(task.name) }
      .filter { task => task.dependencies.forall(isCompleted) }
      .map(_.name)
      .sorted

  val active = availableTasks.isEmpty == false || runningTasks.isEmpty == false

  private def isRunning(task: Char): Boolean = {
    startedTasks.get(task) match {
      case Some(endTime) => endTime > this.time
      case None => false
    }
  }

  private def isCompleted(task: Char): Boolean =
    startedTasks.get(task) match {
      case Some(endTime) => endTime <= this.time
      case None => false
    }

  private def taskDuration(task: Char): Int = defaultDuration + 1 + task.toUpper.toInt - 'A'.toInt

  def startMaximumNumberOfTasks(numberOfTasks: Int): TimeInstructions =
    this.availableTasks.take(numberOfTasks)
      .foldLeft(this) { case (instructions, task) => instructions.startTask(task) }

  def startTask(task: Char): TimeInstructions = {
    val endTime = time + taskDuration(task)
    this.copy(startedTasks = startedTasks + (task -> endTime))
  }

  def proceedSecond: TimeInstructions =
    this.copy(time = this.time + 1)

}

