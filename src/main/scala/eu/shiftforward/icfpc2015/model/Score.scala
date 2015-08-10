package eu.shiftforward.icfpc2015.model

final case class Score(currentScore: Long = 0,
                       prevLinesCleared: Int = 0,
                       powerPhrasesUsed: Set[PowerPhrase] = Set.empty) {

  def updatePower(powerPhrasesScored: Seq[PowerPhrase]) = {
    val (points, newPhrases) = powerPhrasesScored.foldLeft((0, List.empty[PowerPhrase])) {
      case ((acc, phraseAcc), phrase) =>
        if (powerPhrasesUsed.contains(phrase)) (2 * phrase.length, phraseAcc)
        else (300 + 2 * phrase.length, phrase :: phraseAcc)
    }
    Score(currentScore + points, prevLinesCleared, powerPhrasesUsed ++ newPhrases)
  }

  def updateMove(sizeOfCurrentUnit: Int, linesCleared: Int) = {
    val points = sizeOfCurrentUnit + 100 * (1 + linesCleared) * linesCleared / 2
    val lineBonus = if (prevLinesCleared > 1) (prevLinesCleared - 1) * points / 10 else 0
    Score(currentScore + points + lineBonus, linesCleared, powerPhrasesUsed)
  }
}
