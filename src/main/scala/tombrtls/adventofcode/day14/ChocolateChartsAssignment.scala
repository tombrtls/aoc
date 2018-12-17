package tombrtls.adventofcode.day14

object ChocolateChartsAssignment {

  def valueToList(value: Int): List[Int] =
    s"${value}".map(_.toString.toInt).toList

  def rollIndex(index: Int, max: Int): Int = {
    if (index >= max) {
      index % max
    } else {
      index
    }
  }

  def scoreAfterRecipes(recipeCount: Int): Array[Int] = {
    var scoreboard = new Array[Int]((recipeCount + 10) * 2)
    scoreboard.update(0, 3)
    scoreboard.update(1, 7)
    var firstElf = 0
    var secondElf = 1

    def scoreForElves: Int = scoreboard(firstElf) + scoreboard(secondElf)

    var lastRecipeIndex = 2
    for (i <- 2 until recipeCount + 10) {
      for (recipe <- valueToList(scoreForElves)) {
        scoreboard.update(lastRecipeIndex, recipe)
        lastRecipeIndex += 1
      }

      firstElf = rollIndex(firstElf + scoreboard(firstElf) + 1, lastRecipeIndex)
      secondElf = rollIndex(secondElf + scoreboard(secondElf) + 1, lastRecipeIndex)
    }

    scoreboard.drop(recipeCount).take(10)
  }

  def scoresBeforeRecipeWithSameScoreAsInput(recipeCount: Int): Int = {
    var scoreboard = new Array[Int](recipeCount * 100)
    scoreboard.update(0, 3)
    scoreboard.update(1, 7)

    val recipeCountAsString = valueToList(recipeCount)
    var firstElf = 0
    var secondElf = 1

    def scoreForElves: Int = scoreboard(firstElf) + scoreboard(secondElf)

    var lastRecipeIndex = 2
    var foundInput = false
    var inputIndex = 0
    while (foundInput == false) {
      for (recipe <- valueToList(scoreForElves)) {
        scoreboard.update(lastRecipeIndex, recipe)
        lastRecipeIndex += 1

        if (scoreboard.slice(lastRecipeIndex - recipeCountAsString.length, lastRecipeIndex).sameElements(recipeCountAsString)) {
          foundInput = true
          inputIndex = lastRecipeIndex
        }
      }

      firstElf = rollIndex(firstElf + scoreboard(firstElf) + 1, lastRecipeIndex)
      secondElf = rollIndex(secondElf + scoreboard(secondElf) + 1, lastRecipeIndex)
    }

    inputIndex - recipeCountAsString.length
  }

  def main(args: Array[String]): Unit = {
    for (count <- Seq(5, 9, 18, 2018, 890691)) {
      println(s"After ${count}: ${scoreAfterRecipes(count).mkString("")}")
    }

    println("")

    for (count <- Seq(5, 9, 18, 2018, 890691)) {
      println(s"Before ${count}: ${scoresBeforeRecipeWithSameScoreAsInput(count)}")
    }
  }
}
