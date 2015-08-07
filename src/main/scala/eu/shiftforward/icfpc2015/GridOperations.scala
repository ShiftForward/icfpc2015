package eu.shiftforward.icfpc2015

import eu.shiftforward.icfpc2015.model.{ Grid, MoveCommand }

trait GridOperations {

  def move(unit: UnitPos, action: MoveCommand, board: Grid): Option[UnitPos] = {
    Some(unit)
  }

  def lockCell(unit: UnitPos, board: Grid): Grid = {
    board
  }
}
