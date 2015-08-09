package eu.shiftforward.icfpc2015.solver

import eu.shiftforward.icfpc2015._
import eu.shiftforward.icfpc2015.model._
import scala.collection.mutable

trait Solver {
  def play(initialState: GameState, timeLimit: Option[Int] = None): Seq[Command]
}

case class PowerPhraseDiscoverySolver(powerPhrase: String) extends Solver {
  def play(initialState: GameState, timeLimit: Option[Int] = None) = {
    initialState.nextState(powerPhrase).status match {
      case GameState.Failed =>
        throw new Exception("This problem cannot be used to test the specified phrase of power!")
      case _ => Command.string(powerPhrase)
    }
  }
}

object NaivePowerPhrasesSolver extends Solver {

  def play(initialState: GameState, timeLimit: Option[Int] = None) = {
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

class SmartSolver(hp: Array[Double] = SmartSolver.defaultHp,
                  debugOnGameOver: Boolean = true) extends Solver {
  /**
   * The list of commands to test when an unit is to be locked in place.
   */
  private[this] val lockCommandCandidates =
    Seq(MoveW, MoveE, MoveSW, MoveSE, RotateCW, RotateCCW).map(Command.action)

  def play(initialState: GameState, timeLimit: Option[Int] = None): Seq[Command] = {

    val safeTimeLimitMillis = timeLimit.map(_ * 1000 - 1000) // minus 1s to return and calculate PowerPhrase.getBestString
    val startTimeMillis = System.currentTimeMillis()

    def elapsedTime() = System.currentTimeMillis() - startTimeMillis

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

    val numUnits = initialState.units.size

    // when the time comes to try power phrases, we prefer unused phrases as they are worth more points
    def allPowerPhrases = unusedPowerPhrases.toIterator ++ usedPowerPhrases.toIterator

    def playAux(state: GameState): Seq[Command] = {
      safeTimeLimitMillis.map(elapsedTime > _) match {
        case Some(true) => // no time left
          state.commandHistory
        case _ =>
          state.status match {

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
              val pathFinder = new PathFinder(state.grid, state.unitPosState.get.unitPos)

              // retrieve all possible final destinations for the current unit
              val unorderedCandidates = possibleTargets(state)

              // list the candidates ordered from the best to the worst, regardless of whether there a path to there or not
              val candidateCostFunc = { unitPos: UnitPos => cost(state.grid.filled(unitPos.cells.toSeq: _*), numUnits - state.placedUnits) }
              // val candidates = unorderedCandidates.sortBy(candidateCostFunc)
              val candidates =
                if (unorderedCandidates.isEmpty) Stream.empty
                else unorderedCandidates.minBy(candidateCostFunc) #:: unorderedCandidates.sortBy(candidateCostFunc).tail

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
                      currState.nextState(currentPath)
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
                  playAux(lockUnit(optimizeForPower(state, path, allPowerPhrases)))

                case None =>
                  // the unit has nowhere to go, we lock it and continue
                  playAux(lockUnit(state))
              }
          }
      }
    }

    // play! :)
    PowerPhrase.getBestString(playAux(initialState), powerPhrases).map(Command.char)
  }

  val hpMatrix = hp.toList.sliding(6, 6).map(_.toArray).toArray
  private[this] def dot(x: Array[Double], y: Array[Double]) = {
    var acc = 0.0
    (0 until math.min(x.length, y.length)).foreach { case i => acc += x(i) * y(i) }
    acc
  }
  /**
   * Returns the cost of a grid. Lower values correspond to better grids.
   */
  def cost(grid: Grid, remainingUnits: Int): Double = {
    val baseFeatures = Array[Double](grid.aggHeight, grid.bumpiness, grid.holes, grid.fullLines, grid.aggLow, grid.highLow)
    val weighters = Array[Double](1.0, grid.aggHeight, grid.bumpiness, grid.holes, grid.fullLines, grid.aggLow, grid.highLow, remainingUnits)
    dot(hpMatrix.map { hp => dot(hp, baseFeatures) }, weighters)
  }

  /**
   * Returns a command that locks the current unit in place.
   */
  def getLockCommand(grid: Grid, unitPos: Option[UnitPos]) = unitPos.flatMap { pos =>
    lockCommandCandidates.find { comm => GridOperations.transform(pos, comm, grid).isEmpty }
  }

  def lockUnit(state: GameState): GameState = {
    getLockCommand(state.grid, state.currentUnitPos) match {
      case Some(cmd) => state.nextState(cmd)
      case None =>
        throw new Exception("Expecting to lock but could to sherlock...\nCommand History:" +
          state.commandHistory.mkString)
    }
  }

  /**
   * Returns a stream of valid destinations for the current unit. The returned stream is not ordered by grid cost.
   */
  def possibleTargets(state: GameState): Stream[UnitPos] = {
    state.currentUnitPos match {
      case None => Stream.empty
      case Some(cUnit) =>
        for {
          rotatedCUnit <- Stream.iterate(cUnit) { prev => GridOperations.transformUnitPos(prev, RotateCW) }.take(5)
          (Cell(leftCell, topCell), Cell(rightCell, bottomCell)) = rotatedCUnit.unit.boundingBox
          left = 0 - (leftCell - rotatedCUnit.unit.pivot.col)
          right = state.grid.width - 1 - (rightCell - rotatedCUnit.unit.pivot.col)
          top = cUnit.pos.row
          bottom = state.grid.height - 1 - (bottomCell - rotatedCUnit.unit.pivot.row)
          row <- (bottom to top by -1).toStream
          col <- (right to left by -1).toStream
          movedCUnit = rotatedCUnit.copy(pos = Cell(col, row))
          if GridOperations.fits(movedCUnit, state.grid)
          if movedCUnit.kernel.exists { cell => !GridOperations.cellFits(cell, state.grid) }
        } yield movedCUnit
    }
  }
}

object SmartSolver {
  val defaultHp = Array(-0.23, -0.01, -0.6, -0.93, 0.27, 0.97, -0.69, -0.83, 0.85, -0.42, 0.35, -0.24, 0.38, 0.16, -0.46, -0.85, 0.58, 0.87, 0.32, 0.98, 0.26, -0.31, -0.95, -0.7, -0.98, -0.44, -0.48, -0.16, -0.3, -0.91, 0.07, -0.66, 0.88, -0.96, -0.07, -0.06, -0.96, 0.57, 1.0, 0.74, -0.71, 0.93, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
}
