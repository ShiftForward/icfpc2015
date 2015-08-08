package eu.shiftforward.icfpc2015.solver

import eu.shiftforward.icfpc2015.{ GameState, GridOperations, UnitPos }
import eu.shiftforward.icfpc2015.GameState._
import eu.shiftforward.icfpc2015.GridOperations._
import eu.shiftforward.icfpc2015.model.{ Cell, Command }
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

  val commandsToTest =
    Seq(
      Command('p'),
      Command('b'),
      Command('a'),
      Command('l'),
      Command('d'),
      Command('k'))

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
          row <- (0 until state.grid.height).toStream
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
