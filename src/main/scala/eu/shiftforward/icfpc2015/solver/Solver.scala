package eu.shiftforward.icfpc2015.solver

import eu.shiftforward.icfpc2015._
import eu.shiftforward.icfpc2015.model._

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

  def play(initialState: GameState) = {
    val wordsIter = Iterator.continually(PowerPhrase.knownPhrases).flatten.map(_.toList)

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

final case class HyperParameters(a: Double = 0.8729920392163335,
                                 b: Double = 0.20182564826293325,
                                 c: Double = 0.9400811696297673,
                                 d: Double = 0.3790167174127723,
                                 e: Double = -0.34908895269497164,
                                 f: Double = 0.16749221845450513)

// TODO naming!
class SmartSolver(hp: HyperParameters = HyperParameters(), debugOnGameOver: Boolean = true) extends Solver {

  def cost(grid: Grid): Double =
    hp.a * grid.aggHeight +
      hp.b * grid.bumpiness +
      hp.c * grid.holes +
      hp.d * grid.fullLines +
      hp.e * grid.aggLow +
      hp.f * grid.highLow

  def play(initialState: GameState): Seq[Command] = {
    def playAux(state: GameState, addLock: Boolean = false): Seq[Command] = state.status match {

      case GameState.GameOver =>
        if (debugOnGameOver) {
          println("GAME OVER")
          println(GameStateRenderer.stateAsString(state))
        }

        state.commandHistory

      case GameState.Failed =>
        throw new Exception("SmartSolver led to a failure state!\nCommand History:" + state.commandHistory.map(_.ch).mkString)

      case GameState.Running =>
        lazy val lockCommand = getLockCommand(state.grid, state.currentUnitPos)
        if (addLock && lockCommand.isDefined) playAux(state.nextState(lockCommand.get))
        else if (addLock) {
          GameStateRenderer.stateAsString(state)
          println(state.unitPosState)
          throw new RuntimeException("Expecting to lock but could to sherlock...")
        } else {
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
          col <- (state.grid.width - 1 to 0 by -1).toStream
          row <- (state.grid.height - 1 to cUnit.pos.row by -1).toStream
          movedCUnit = cUnit.copy(pos = Cell(col, row))
          rotatedCUnit <- Stream.iterate(movedCUnit) { prev => GridOperations.transformUnitPos(prev, RotateCW) }.take(5)
          if GridOperations.fits(rotatedCUnit, state.grid)
          if rotatedCUnit.kernel.exists { cell => !GridOperations.cellFits(cell, state.grid) }
        } yield rotatedCUnit
    }
  }
}
