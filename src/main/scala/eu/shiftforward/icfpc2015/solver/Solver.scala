package eu.shiftforward.icfpc2015.solver

import eu.shiftforward.icfpc2015._
import eu.shiftforward.icfpc2015.GridOperations._
import eu.shiftforward.icfpc2015.model._
import scala.collection.mutable

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
    val wordsIter = Iterator.continually(knownWords).flatten.map(_.toList)

    def fillUntilGameOver(state: GameState,
                          nextCommands: List[Char],
                          prevWordState: GameState): Seq[Command] = {

      state.status match {
        case GameState.GameOver => state.commandHistory

        case GameState.Failed =>
          // revert to the state at the end of the previous word and try the next power phrase
          fillUntilGameOver(prevWordState, wordsIter.next(), prevWordState)

        case GameState.Running => nextCommands match {
          case Nil => fillUntilGameOver(state, wordsIter.next(), state)
          case next :: tail => fillUntilGameOver(state.nextState(next), tail, prevWordState)
        }
      }
    }

    fillUntilGameOver(initialState, wordsIter.next(), initialState)
  }
}

// TODO naming!
class SmartSolver(a: Double = 0.51, b: Double = 0.18, c: Double = 0.36, d: Double = -0.76) extends Solver {

  def cost(grid: Grid): Double =
    a * grid.aggHeight +
      b * grid.bumpiness +
      c * grid.holes +
      d * grid.fullLines

  def play(initialState: GameState): Seq[Command] = {
    def playAux(state: GameState, addLock: Boolean = false): Seq[Command] = state.status match {

      case GameState.GameOver =>
        println("GAME OVER")
        println(GameStateRenderer.stateAsString(state))
        state.commandHistory

      case GameState.Failed =>
        throw new Exception("SmartSolver led to a failure state!\nCommand History:" + state.commandHistory.map(_.ch).mkString)

      case GameState.Running =>
        lazy val lockCommand = getLockCommand(state.grid, state.currentUnitPos)
        if (addLock && lockCommand.isDefined) playAux(state.nextState(lockCommand.get))
        else {
          val candidates = possibleTargets(state).sortBy { newUnitPos =>
            val newGrid = state.grid.filled(newUnitPos.cells.toSeq: _*)
            cost(newGrid)
          }

          val pathFinder = new PathFinder(state.grid, state.unitPosState.get.unitPos)

          candidates.flatMap(t => pathFinder.pathTo(t)).headOption match {
            case Some(p) =>
              playAux(state.nextState(p), addLock = true)
            case None =>
              if (lockCommand.isDefined) playAux(state.nextState(lockCommand.get))
              else {
                println("SmartSolver could not reach game over!\nCommand History:" + state.commandHistory.map(_.ch).mkString)
                state.commandHistory
              }
          }
        }
    }

    // this is here so that problem 14 terminates
    if (initialState.grid.width <= 25) playAux(initialState)
    else NaivePowerPhrasesSolver.play(initialState)
  }

  val commandsToTest =
    Seq(
      Command('p'),
      Command('b'),
      Command('a'),
      Command('l'),
      Command('d'),
      Command('k'))

  def getLockCommand(grid: Grid, unitPos: Option[UnitPos]) = unitPos.flatMap { pos =>
    commandsToTest.find { comm => GridOperations.transform(pos, comm, grid).isEmpty }
  }

  def possibleTargets(state: GameState): Stream[UnitPos] = {
    state.currentUnitPos match {
      case None => Stream.empty
      case Some(cUnit) =>
        for {
          // TODO this positions probably should take the bounding box and pivot into account
          col <- (0 until state.grid.width).toStream
          row <- (cUnit.pos.row until state.grid.height).toStream
          newCUnit = cUnit.copy(pos = Cell(col, row))
          if GridOperations.fits(newCUnit, state.grid)
          if newCUnit.kernel.exists { cell => !GridOperations.cellFits(cell, state.grid) }
        } yield newCUnit
    }
  }
}
