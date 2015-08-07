package eu.shiftforward.icfpc2015.model

class PowerPhrase(text: List[Char]) {
  val movements = text.map(Command.char)
  val length = text.length
}

object PowerPhrase {
  def getMatching(source: List[Command], powerphrases: List[PowerPhrase]): Map[PowerPhrase, List[Int]] = {

    var matching = List[(Int, PowerPhrase)]()
    var matched = Map[PowerPhrase, List[Int]]()

    for (i <- source.indices) {
      val command = source(i)

      powerphrases.foreach { target =>
        matching = matching :+ (0, target)
      }

      matching = matching.foldLeft(List[(Int, PowerPhrase)]()) {
        case (acc, (idx, power)) =>

          if (command == power.movements(idx)) {
            if (idx + 1 == power.movements.length) {
              val startingIdx = i - power.movements.length + 1

              matched = matched.updated(power, matched.getOrElse(power, List[Int]()) :+ startingIdx)
              acc
            } else {
              acc :+ (idx + 1, power)
            }
          } else {
            acc
          }
      }
    }
    matched
  }
}

