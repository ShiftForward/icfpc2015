package eu.shiftforward.icfpc2015.solver

import eu.shiftforward.icfpc2015.GameState
import eu.shiftforward.icfpc2015.model.Command

trait Solver {
  def play(initialState: GameState): Seq[Command]
}

object NaivePowerPhrasesSolver extends Solver {

  // right now this is only for documentation
  val knownWords = List(
    "Ei!", // from statement
    "Ia! Ia!", // (?) from problem 3 grid
    "R'lyeh") // (?) from problem 5 grid

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
