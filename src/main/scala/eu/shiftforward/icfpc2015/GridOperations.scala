package eu.shiftforward.icfpc2015

import eu.shiftforward.icfpc2015.model.{ Cell, CellUnit, Grid, MoveCommand }

trait GridOperations {

  def move(unit: UnitPos, action: MoveCommand, board: Grid): Option[UnitPos] = {
    Some(unit)
  }

  def lockCell(unit: UnitPos, board: Grid): Grid = {
    board
  }

  def initialPosition(unit: CellUnit, board: Grid): Option[UnitPos] = {
    Some(UnitPos(unit, Cell(0, 0)))
  }

  def cellIntercepts(unit: UnitPos, board: Grid): Boolean = {
    false
  }

  def moveUnitPos(unit: UnitPos, action: MoveCommand): UnitPos = {
    unit
  }
}
