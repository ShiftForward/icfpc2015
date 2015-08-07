package eu.shiftforward.icfpc2015

import eu.shiftforward.icfpc2015.model._

case class GameState(grid: Grid, units: Iterator[CellUnit], currentUnitPos: Option[UnitPos], gameOver: Boolean, prevStates: Set[UnitPos] = Set()) extends GridOperations {
  def getNextUnitPos(nextGrid: Grid): GameState = {
    if (!units.hasNext)
      GameState(nextGrid, units, None, gameOver = true)
    else {
      initialPosition(units.next(), grid) match {
        case None =>
          GameState(nextGrid, units, None, gameOver = true)
        case nextPos =>
          GameState(nextGrid, units, nextPos, gameOver = false, prevStates = Set(nextPos.get))
      }
    }
  }

  def nextState(move: Char): GameState = currentUnitPos match {
    case Some(pos) =>
      val command = Command(move)
      transform(pos, command, grid) match {
        case None =>
          val nextGrid = removeLines(lockCell(pos, grid))
          getNextUnitPos(nextGrid)
        case nextPosOpt @ Some(nextPos) =>
          if (validateTransform(nextPos, prevStates))
            GameState(grid, units, nextPosOpt, gameOver = false, prevStates + nextPos)
          else
            GameState(grid, units, None, gameOver = true)
      }
    case None =>
      getNextUnitPos(grid)
  }

  def nextState(moves: String): GameState =
    if (moves.isEmpty) this
    else nextState(moves.head).nextState(moves.tail)

  def start() = getNextUnitPos(grid)
}

object GameState extends GridOperations {
  def apply(grid: Grid, units: Iterator[CellUnit]): GameState = {
    GameState(grid, units, None, gameOver = false).start()
  }
}
