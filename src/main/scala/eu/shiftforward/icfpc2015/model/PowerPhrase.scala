package eu.shiftforward.icfpc2015.model

class PowerPhrase(text: List[Char]) {
  val movements = text.map(Command.char)
  val length = text.length
}

object PowerPhrase {
  def _match(source: List[Command], powerphrases: List[PowerPhrase]) = {

    var matching = List[(Int, PowerPhrase)]()
    var matched = List[(Int, PowerPhrase)]()

    for (i <- source.indices) {
      val command = source(i)

      powerphrases.foreach { target =>
        matching = matching :+ (0, target)
      }

      matching = matching.foldLeft(List[(Int, PowerPhrase)]()) {
        case (acc, m @ (idx, power)) =>

          if (command == power.movements(idx)) {
            if (idx + 1 == power.movements.length) {
              matched = matched :+ (i - power.movements.length + 1, power)
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

  def _print
}

