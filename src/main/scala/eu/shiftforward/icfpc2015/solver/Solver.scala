package eu.shiftforward.icfpc2015.solver

import eu.shiftforward.icfpc2015.model.{ Cell, Command }
import eu.shiftforward.icfpc2015.{ GameState, GridOperations, UnitPos }

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
    val commandsIter = Iterator.continually(knownWords.mkString.toIterator).flatten

    def fillUntilGameOver(state: GameState, prevState: GameState = null, history: List[Char] = Nil): List[Char] =
      if (state.gameOver && state.score.currentScore == 0) {
        // revert to the previous state and try with the next command
        fillUntilGameOver(prevState, null, history.tail)
      } else if (state.gameOver) {
        history.reverse
      } else {
        val next = commandsIter.next()
        fillUntilGameOver(state.nextState(next), state, next :: history)
      }

    fillUntilGameOver(initialState).map(Command.char)
  }
}

object SmartSolver extends Solver {
  def play(initialState: GameState): Seq[Command] = {
    def playAux(state: GameState, commands: Seq[Command]): Seq[Command] =
      if (state.gameOver) commands
      else {
        possibleTargets(initialState).map(t => findPath(initialState, t)).find(_.isEmpty).flatten match {
          case Some(p) => playAux(state.nextState(p), commands ++ p)
          case None => commands
        }
      }

    playAux(initialState, Nil)
  }

  def findPath(state: GameState, dst: UnitPos): Option[List[Command]] = {
    None
  }

  def possibleTargets(state: GameState): Stream[UnitPos] = {
    state.currentUnitPos match {
      case None => Stream.empty
      case Some(cUnit) =>
        for {
          col <- (0 until state.grid.width).toStream
          row <- (0 until state.grid.height).toStream

          newCUnit = cUnit.copy(pos = Cell(col, row))

          piece <- Stream.iterate(Option(newCUnit)) {
            case None => None
            case Some(e) => GridOperations.transform(e, Command('d'), state.grid)
          }.take(6)

          p <- piece

          if GridOperations.fits(p, state.grid)
        } yield p
    }
  }
}