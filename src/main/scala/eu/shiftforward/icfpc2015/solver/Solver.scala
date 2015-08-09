package eu.shiftforward.icfpc2015.solver

import eu.shiftforward.icfpc2015._
import eu.shiftforward.icfpc2015.model._
import scala.collection.mutable

trait Solver {
  def play(initialState: GameState): Seq[Command]
}

case class PowerPhraseDiscoverySolver(powerPhrase: String) extends Solver {
  def play(initialState: GameState) = {
    initialState.nextState(powerPhrase).status match {
      case GameState.Failed =>
        throw new Exception("This problem cannot be used to test the specified phrase of power!")
      case _ => Command.string(powerPhrase)
    }
  }
}

object NaivePowerPhrasesSolver extends Solver {

  def play(initialState: GameState) = {
    val wordsIter = Iterator.continually(initialState.powerPhrases).flatten.map(_.text)

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
class SmartSolver(hp: Array[Double], debugOnGameOver: Boolean = true) extends Solver {
  def this() {
    this(Array[Double](
      0.21, 0.74, 0.96, 0.72, -0.8, 0.19, 0.7, 0.69, 0.61, 0.17, 0.08, -0.9, 0.87, -0.49, -0.64, -0.84, 0.74, 0.57, 0.52, -0.51, -0.26, -0.85, -0.24, 0.45, 0.49, -0.46, 0.54, 0.92, -0.06, -0.75, 0.64, 0.66, 0.59, 0.79, -0.15, 0.88, -0.23, 0.01, 0.04, -0.59, -0.19, -0.49))
  }

  /**
   * The list of commands to test when an unit is to be locked in place.
   */
  private[this] val lockCommandCandidates =
    Seq(MoveW, MoveE, MoveSW, MoveSE, RotateCW, RotateCCW).map(Command.action)

  def play(initialState: GameState): Seq[Command] = {

    // The power phrases to optimize commands for. This value can be set to a subset of the known phrases in order to
    // improve performance or it can even be set to `Nil` to disable power phrase optimization
    val powerPhrases = initialState.powerPhrases

    // A map from power phrases to their list of respective commands and its compiled transformation
    val powerPhraseIndex = powerPhrases.map { phrase =>
      val commands = phrase.movements :+ Command.action(MoveSE) // MoveSE in order to force valid paths
      val transform = GridOperations.compileTransform(commands.map(_.action))
      phrase -> (commands, transform)
    }.toMap

    // for each game, we keep track of the power phrases we already used
    val unusedPowerPhrases = mutable.HashSet(powerPhrases: _*)
    val usedPowerPhrases = mutable.HashSet.empty[PowerPhrase]

    // when the time comes to try power phrases, we prefer unused phrases as they are worth more points
    def allPowerPhrases = unusedPowerPhrases.toIterator ++ usedPowerPhrases.toIterator

    def playAux(state: GameState, addLock: Boolean = false): Seq[Command] = state.status match {

      case GameState.GameOver =>
        if (debugOnGameOver) {
          println("GAME OVER")
          println(GameStateRenderer.stateAsString(state))
        }
        state.commandHistory

      case GameState.Failed =>
        throw new Exception("SmartSolver led to a failure state!\nCommand History:" +
          state.commandHistory.mkString)

      case GameState.Running =>
        lazy val lockCommand = getLockCommand(state.grid, state.currentUnitPos)
        if (addLock && lockCommand.isDefined) playAux(state.nextState(lockCommand.get))
        else if (addLock) {
          println(GameStateRenderer.stateAsString(state))
          println(state.unitPosState)
          throw new RuntimeException("Expecting to lock but could to sherlock...")
        } else {
          val pathFinder = new PathFinder(state.grid, state.unitPosState.get.unitPos)

          // list the candidates ordered from the best to the worst, regardless of whether there a path to there or not
          //val candidates = possibleTargets(state).sortBy { newUnitPos =>
          val candidates = Utils.insertionSortBy(possibleTargets(state), { newUnitPos: UnitPos =>
            val newGrid = state.grid.filled(newUnitPos.cells.toSeq: _*)
            cost(newGrid)
          })

          // filter out the candidates without a valid path to there, keep both the destination and the path found
          val validCandidates = candidates.flatMap { dest =>
            pathFinder.pathTo(dest).map { path => (dest, path) }
          }

          // select the best valid candidate - the final destination of the unit, `dest`, will not change anymore after
          // this
          validCandidates.headOption match {
            case Some((dest, path)) =>
              val revPathFinder = new ReversePathFinder(state.grid, dest)
              // println(s"dest = $dest")

              // between the unit's initial position and its destination, try to use as many power phrases as possible
              def optimizeForPower(currState: GameState,
                                   currentPath: Seq[Command],
                                   powerPhrasesToTry: Iterator[PowerPhrase]): GameState = {
                if (powerPhrasesToTry.isEmpty) {
                  // if there are no more power words to try, stop optimizing and execute the previously calculated
                  // shortest path
                  currState.nextState(PowerPhrase.getBestString(currentPath, currState.powerPhrases))
                } else {
                  // obtain the next power phrase to try
                  val powerPhrase = powerPhrasesToTry.next()
                  val (powerCommands, powerTransform) = powerPhraseIndex(powerPhrase)

                  // obtain the position the unit will be after the power phrase (possibly out of the grid)
                  val unitPosAfterPower = GridOperations.transformUnitPos(
                    currState.unitPosState.get.unitPos, powerTransform)

                  // println(s"revPathFinder.pathFrom($unitPosAfterPower) = ${revPathFinder.pathFrom(unitPosAfterPower)}")

                  // find a path from that position to the destination
                  revPathFinder.pathFrom(unitPosAfterPower) match {
                  // new PathFinder(state.grid, unitPosAfterPower).pathTo(dest) match { // slow, need `revPathFinder`!
                    case Some(pathAfterPower) =>
                      // if there is a path, obtain the game state after applying the power commands
                      val newState = currState.nextState(powerCommands)

                      // the previously calculated position does not take in account locked cells during the power
                      // phrase commands, only the final position. Test here if the game entered a non-running state or
                      // the unit we're dealing with was locked
                      if (newState.status != GameState.Running || newState.units.length != currState.units.length) {
                        // if something happened, try the next power phrase
                        optimizeForPower(currState, currentPath, powerPhrasesToTry)
                      } else {
                        // else, record usage of power phrase and continue optimizing from the new state
                        unusedPowerPhrases -= powerPhrase
                        usedPowerPhrases += powerPhrase
                        optimizeForPower(newState, pathAfterPower, allPowerPhrases)
                      }

                    case None =>
                      // if there is not any path, try the next power phrase
                      optimizeForPower(currState, currentPath, powerPhrasesToTry)
                  }
                }
              }

              playAux(optimizeForPower(state, path, allPowerPhrases), addLock = true)

            case None =>
              if (lockCommand.isDefined) playAux(state.nextState(lockCommand.get))
              else {
                println("SmartSolver could not reach game over!\nCommand History:" +
                  state.commandHistory.mkString)
                state.commandHistory
              }
          }
        }
    }

    playAux(initialState)

    // this is here so that problem 14 terminates
    // if (initialState.grid.width <= 25) playAux(initialState)
    // else NaivePowerPhrasesSolver.play(initialState)
  }

  /**
   * Returns the cost of a grid. Lower values correspond to better grids.
   */

  def cost(grid: Grid): Double = {
    (hp(36) + hp(0) * grid.aggHeight + hp(1) * grid.bumpiness + hp(2) * grid.holes + hp(3) * grid.fullLines + hp(4) * grid.aggLow + hp(5) * grid.highLow) * grid.aggHeight +
      (hp(37) + hp(6) * grid.aggHeight + hp(7) * grid.bumpiness + hp(8) * grid.holes + hp(9) * grid.fullLines + hp(10) * grid.aggLow + hp(11) * grid.highLow) * grid.bumpiness +
      (hp(38) + hp(12) * grid.aggHeight + hp(13) * grid.bumpiness + hp(14) * grid.holes + hp(15) * grid.fullLines + hp(16) * grid.aggLow + hp(17) * grid.highLow) * grid.holes +
      (hp(39) + hp(18) * grid.aggHeight + hp(19) * grid.bumpiness + hp(20) * grid.holes + hp(21) * grid.fullLines + hp(22) * grid.aggLow + hp(23) * grid.highLow) * grid.fullLines +
      (hp(40) + hp(24) * grid.aggHeight + hp(25) * grid.bumpiness + hp(26) * grid.holes + hp(27) * grid.fullLines + hp(28) * grid.aggLow + hp(29) * grid.highLow) * grid.aggLow +
      (hp(41) + hp(30) * grid.aggHeight + hp(31) * grid.bumpiness + hp(32) * grid.holes + hp(33) * grid.fullLines + hp(34) * grid.aggLow + hp(35) * grid.highLow) * grid.highLow
  }

  /**
   * Returns a command that locks the current unit in place.
   */
  def getLockCommand(grid: Grid, unitPos: Option[UnitPos]) = unitPos.flatMap { pos =>
    lockCommandCandidates.find { comm => GridOperations.transform(pos, comm, grid).isEmpty }
  }

  /**
   * Returns a stream of valid destinations for the current unit. The returned stream is not ordered by grid cost.
   */
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
