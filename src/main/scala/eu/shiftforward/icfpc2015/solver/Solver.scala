package eu.shiftforward.icfpc2015.solver

import eu.shiftforward.icfpc2015.{ GameStateRenderer, GameState, GridOperations, UnitPos }
import eu.shiftforward.icfpc2015.GameState._
import eu.shiftforward.icfpc2015.GridOperations._
import eu.shiftforward.icfpc2015.model.{ Grid, Cell, Command }
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
class SmartSolver(a: Double = -3.0, b: Double = -1.0, c: Double = -1.0, d: Double = 100.0) extends Solver {

  def reward(grid: Grid): Double =
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
        throw new Exception("SmartSolver led to a failure state!")

      case GameState.Running =>
        lazy val lockCommand = getLockCommand(state.grid, state.currentUnitPos)
        if (addLock && lockCommand.isDefined) playAux(state.nextState(lockCommand.get))
        else {
          possibleTargets(state).sortBy { newUnitPos =>
            val newGrid = state.grid.filled(newUnitPos.cells.toSeq: _*)
            reward(newGrid)
          }.reverse
            .flatMap(t => findPath(state, t)).headOption match {
              case Some(p) =>
                playAux(state.nextState(p), addLock = true)
              case None =>
                if (lockCommand.isDefined) playAux(state.nextState(lockCommand.get))
                else {
                  println("NO PATHS FOUND")
                  println(GameStateRenderer.stateAsString(state))
                  state.commandHistory
                }
            }
        }
    }

    playAux(initialState)
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
    commandsToTest
      .toStream
      .flatMap { comm =>
        GridOperations.transform(pos, comm, grid) match {
          case None => Some(comm)
          case Some(_) => None
        }
      }.headOption

  }

  def findPath(state: GameState, dst: UnitPos): Option[List[Command]] = {
    state.currentUnitPos match {
      case Some(startPos) =>
        implicit val ordering = new Ordering[(Int, UnitPos)] {
          def compare(p1: (Int, UnitPos), p2: (Int, UnitPos)) = p1._1 compare p2._1
        }.reverse

        val pq = mutable.PriorityQueue[(Int, UnitPos)]()
        val prev = mutable.Map[UnitPos, (UnitPos, Command, Int)]()
        val grid = state.grid
        pq.enqueue((0, startPos))

        def loop() {
          if (!pq.isEmpty) {
            val (_, currentPos) = pq.dequeue
            val dist = if (currentPos == startPos) 0 else prev(currentPos)._3
            if (currentPos != dst) {
              commandsToTest.foreach { command =>
                transform(currentPos, command, grid).foreach { nextPos =>
                  if (!prev.contains(nextPos) && nextPos != startPos) {
                    prev.update(nextPos, (currentPos, command, dist + 1))
                    pq.enqueue((dist + 1 + nextPos.pos.distance(dst.pos), nextPos))
                  }
                }
              }
              loop()
            }
          }
        }

        loop()

        prev.get(dst) match {
          case Some(_) =>
            val c = mutable.ListBuffer[Command]()
            def go(p: UnitPos) {
              prev.get(p) match {
                case Some((pr, comm, _)) =>
                  c += comm
                  go(pr)
                case None => // do nothing
              }
            }

            go(dst)
            Some(c.toList.reverse)

          case None =>
            None
        }

      case None => None
    }
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
          /*piece <- Stream.iterate(Option(newCUnit)) {
            case None => None
            case Some(e) => GridOperations.transform(e, Command('d'), state.grid)
          }.take(6)
          p <- piece
          if GridOperations.fits(p, state.grid)
          */
          if GridOperations.fits(newCUnit, state.grid)
          if newCUnit.kernel.exists { cell => !GridOperations.cellFits(cell, state.grid) }
        } yield newCUnit
    }
  }
}
