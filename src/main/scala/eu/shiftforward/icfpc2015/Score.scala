package eu.shiftforward.icfpc2015

final case class Score(currentScore: Long = 0, prevLinesCleared: Int = 0) {
  def update(sizeOfCurrentUnit: Int, linesCleared: Int) = {
    val points = sizeOfCurrentUnit + 100 * (1 + linesCleared) * linesCleared / 2
    val lineBonus = if (prevLinesCleared > 1) (prevLinesCleared - 1) * points / 10 else 0
    Score(currentScore + points + lineBonus, linesCleared)
  }
}
