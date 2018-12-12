package tombrtls.adventofcode.day7

import tombrtls.adventofcode.Assignment

case class Dependency(dependency: String, task: String)

object SumOfPartsAssignment1 extends Assignment[Seq[Dependency], String] {
  def main(args: Array[String]): Unit = startAssignment

  override val day: Int = 7
  override val testCases: Seq[(String, String)] = Seq(
    ("sample.txt", "CABDFE")
  )

  override val inputFileName: String = "input.txt"

  val regexp = "Step ([a-zA-Z]*) must be finished before step ([a-zA-Z]*) can begin.".r
  override def processLines(lines: Seq[String]): Seq[Dependency] =
    lines.map {
      case regexp(dependency, task) => Dependency(dependency, task)
    }

  override def implementation(input: Seq[Dependency]): String = {
    val tasks = input.map(_.task).toSet
    val dependencies = input.map(_.dependency).toSet
    val startingPoint = dependencies.diff(tasks).head
    val dependentMap = input
      .map { task => (task.dependency, task.task) }
      .groupBy(_._1)
      .mapValues(_.map(_._2))
    def inner(acc: String, tasks: Seq[String]): String = {
      tasks match {
        case x +: xs => {
          val string = acc ++ x
          dependentMap.get(x) match {
            case None => inner(string, xs)
            case Some(followUps) =>  {
              val allTasks = xs ++ followUps
              inner(string, allTasks.sorted)
            }
          }
        }
        case Seq(x) => acc ++ x
        case Seq() => acc
      }

    }
    inner("", Seq(startingPoint))
  }
}
