package eu.shiftforward.icfpc2015

import eu.shiftforward.icfpc2015.model._
import GridOperations._
import GameState._

case class GameState(
    grid: Grid,
    units: Seq[CellUnit],
    unitPosState: Option[UnitPosState],
    status: Status,
    score: Score = Score(),
    commandHistory: Vector[Command] = Vector()) {

  def nextState(move: Char): GameState = nextState(Command.char(move))

  def nextState(command: Command): GameState = {
    val nextCommandHistory = commandHistory :+ command

    def getNextUnitPos(nextGrid: Grid, nextScore: Score): GameState = {
      units match {
        case h :: t =>
          initialPosition(h, nextGrid) match {
            case None =>
              GameState(nextGrid, t, None, GameOver, nextScore, nextCommandHistory)
            case Some(nextPos) =>
              GameState(nextGrid, t, Some(UnitPosState(nextPos, prevStates = Set(nextPos))), status, nextScore, nextCommandHistory)
          }
        case _ =>
          GameState(nextGrid, units, None, GameOver, nextScore, nextCommandHistory)
      }
    }

    unitPosState match {
      case Some(prevState @ UnitPosState(pos, _)) =>
        transform(pos, command, grid) match {
          case None =>
            val (nextGrid, removedLines) = removeLines(lockCell(pos, grid))
            val nextScore = score.update(pos.cells.size, removedLines)
            getNextUnitPos(nextGrid, nextScore)

          case nextPosOpt @ Some(nextPos) =>
            val updatedUnitState = prevState.update(nextPos)
            if (updatedUnitState.valid)
              GameState(grid, units, Some(updatedUnitState), status, score, nextCommandHistory)
            else
              GameState(grid, units, None, Failed, Score(), nextCommandHistory)
        }
      case None =>
        println("We shouldn't have gotten here!")
        getNextUnitPos(grid, score)
    }
  }

  def nextState(moves: String): GameState =
    if (moves.isEmpty) this
    else nextState(moves.head).nextState(moves.tail)

  def nextState(moves: Seq[Command]): GameState =
    if (moves.isEmpty) this
    else nextState(moves.head).nextState(moves.tail)

  def gameOver = status != Running

  def currentUnitPos = unitPosState.map(_.unitPos)
}

object GameState {
  sealed trait Status
  case object Running extends Status
  case object GameOver extends Status
  case object Failed extends Status

  case class UnitPosState(unitPos: UnitPos, prevStates: Set[UnitPos] = Set()) {
    def valid =
      !prevStates.contains(unitPos)

    def update(newUnitPos: UnitPos) =
      UnitPosState(newUnitPos, prevStates + unitPos)
  }

  def apply(grid: Grid, units: Seq[CellUnit]): GameState = {
    units match {
      case Nil =>
        GameState(grid, units, None, GameOver)
      case h :: t =>
        initialPosition(h, grid) match {
          case Some(pos) =>
            GameState(grid, t, Some(UnitPosState(pos, prevStates = Set(pos))), Running)
          case None =>
            GameState(grid, t, None, GameOver)
        }
    }
  }
}
