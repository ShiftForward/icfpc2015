package eu.shiftforward.icfpc2015

import eu.shiftforward.icfpc2015.model._

trait GridOperations {

  def transform(unitPos: UnitPos, command: Command, grid: Grid): Option[UnitPos] = {
    val transformed = transformUnitPos(unitPos, command)

    if (fits(transformed, grid)) Some(transformed)
    else None
  }

  def lockCell(unitPos: UnitPos, grid: Grid): Grid = {
    grid
  }

  def initialPosition(unitPos: CellUnit, grid: Grid): Option[UnitPos] = {
    val (topLeft, bottomRight) = unitPos.boundingBox
    val unitWidth = bottomRight.col - topLeft.col + 1
    val leftShift = Math.floor((grid.width - unitWidth) / 2).toInt
    val x = Math.max(0, leftShift + unitPos.pivot.row)
    val y = Math.max(0, topLeft.col + unitPos.pivot.col)

    val initialCellUnit = UnitPos(unitPos, Cell(x, y))
    if (fits(initialCellUnit, grid)) Some(initialCellUnit)
    else None
  }

  def fits(unitPos: UnitPos, grid: Grid): Boolean = {
    unitPos.cells.forall {
      case Cell(x, y) =>
        x >= 0 && x < grid.width &&
          y >= 0 && y < grid.height &&
          !grid.isFilled(x, y)
    }
  }

  def transformUnitPos(unitPos: UnitPos, command: Command): UnitPos = command.action match {
    case MoveW => unitPos.copy(pos = unitPos.pos.copy(x = unitPos.pos.x - 1))
    case MoveE => unitPos.copy(pos = unitPos.pos.copy(x = unitPos.pos.x + 1))
    case MoveSW => unitPos.copy(pos = unitPos.pos.copy(x = unitPos.pos.x - (if (unitPos.pos.y % 2 == 0) 1 else 0), y = unitPos.pos.y + 1))
    case MoveSE => unitPos.copy(pos = unitPos.pos.copy(x = unitPos.pos.x + (if (unitPos.pos.y % 2 == 0) 0 else 1), y = unitPos.pos.y + 1))
    case RotateCW => unitPos.copy(unit = unitPos.unit.rotateCW)
    case RotateCCW => unitPos.copy(unit = unitPos.unit.rotateCCW)
  }
}
