package eu.shiftforward.icfpc2015

import eu.shiftforward.icfpc2015.model._

case class GameState(
    grid: Grid,
    units: Iterator[CellUnit],
    currentUnitPos: Option[UnitPos],
    gameOver: Boolean,
    score: Score = Score(),
    prevStates: Set[UnitPos] = Set()) extends GridOperations {
  def getNextUnitPos(nextGrid: Grid, nextScore: Score): GameState = {
    if (!units.hasNext)
      GameState(nextGrid, units, None, gameOver = true, nextScore)
    else {
      initialPosition(units.next(), grid) match {
        case None =>
          GameState(nextGrid, units, None, gameOver = true, nextScore)
        case nextPos =>
          GameState(nextGrid, units, nextPos, gameOver = false, nextScore, prevStates = Set(nextPos.get))
      }
    }
  }

  def nextState(move: Char): GameState = currentUnitPos match {
    case Some(pos) =>
      val command = Command(move)
      transform(pos, command, grid) match {
        case None =>
          val (nextGrid, removedLines) = removeLines(lockCell(pos, grid))
          val nextScore = score.update(pos.cells.size, removedLines)
          getNextUnitPos(nextGrid, nextScore)
        case nextPosOpt @ Some(nextPos) =>
          if (validateTransform(nextPos, prevStates))
            GameState(grid, units, nextPosOpt, gameOver = false, score, prevStates + nextPos)
          else
            GameState(grid, units, None, gameOver = true, Score())
      }
    case None =>
      getNextUnitPos(grid, score)
  }

  def nextState(moves: String): GameState =
    if (moves.isEmpty) this
    else nextState(moves.head).nextState(moves.tail)

  def start() = getNextUnitPos(grid, score)
}

object GameState extends GridOperations {
  def apply(grid: Grid, units: Iterator[CellUnit]): GameState = {
    GameState(grid, units, None, gameOver = false).start()
  }
}
