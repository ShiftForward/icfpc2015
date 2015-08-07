package eu.shiftforward.icfpc2015.solver

import eu.shiftforward.icfpc2015.GameState
import eu.shiftforward.icfpc2015.model.Command

trait Solver {
  def play(initialState: GameState): Seq[Command]
}

/**
 * Points for problems 0 to 3 (2015-08-07 20:21):
 *
 * "Ei! " - [383, 0, 152, 326]
 * "Ei!Ia! Ia!R'lyeh" - [0, 0, 316, 501]
 */
object NaivePowerPhrasesSolver extends Solver {

  // right now this is only for documentation
  val knownWords = List(
    "Ei!", // from statement
    "Ia! Ia!", // from problem 3 grid
    "R'lyeh", // from problem 5 grid
    "Yuggoth") // from problem 7 grid

  def play(initialState: GameState) = {
    val commandsIter = Iterator.continually("Ei! ".toIterator).flatten

    def fillUntilGameOver(state: GameState, history: List[Char] = Nil): List[Char] =
      if (state.gameOver) history.reverse
      else {
        val next = commandsIter.next()
        fillUntilGameOver(state.nextState(next), next :: history)
      }

    fillUntilGameOver(initialState).map(Command.char)
  }
}
