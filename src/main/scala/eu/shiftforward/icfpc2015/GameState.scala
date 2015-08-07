package eu.shiftforward.icfpc2015

import eu.shiftforward.icfpc2015.model._

case class GameState(grid: Grid, units: Iterator[CellUnit], currentUnitPos: Option[UnitPos], gameOver: Boolean) extends GridOperations {
  def nextState(move: Char): GameState = currentUnitPos match {
    case Some(pos) =>
      val command = Command(move)
      transform(pos, command, grid) match {
        case None =>
          val nextGrid = removeLines(lockCell(pos, grid))
          GameState(nextGrid, units, initialPosition(units.next(), nextGrid), gameOver = false)
        case nextPos => GameState(grid, units, nextPos, gameOver = false)
      }
    case None =>
      GameState(grid, units, currentUnitPos, gameOver = true)
  }

  def nextState(moves: String): GameState =
    if (moves.isEmpty) this
    else nextState(moves.head).nextState(moves.tail)
}

object GameState extends GridOperations {
  def apply(grid: Grid, units: Iterator[CellUnit]): GameState =
    GameState(grid, units, initialPosition(units.next(), grid), gameOver = false)
}
