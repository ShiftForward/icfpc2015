package eu.shiftforward.icfpc2015.model

import eu.shiftforward.icfpc2015.util.GridOperations

case class CellUnit(members: Set[Cell], pivot: Cell) {
  lazy val rotateCCW = new CellUnit(members.map(_.rotateCCW(pivot)), pivot)

  lazy val rotateCW = new CellUnit(members.map(_.rotateCW(pivot)), pivot)

  lazy val boundingBox: (Cell, Cell) = {
    val topLeft = Cell(members.map(_.col).min, members.map(_.row).min)
    val bottomRight = Cell(members.map(_.col).max, members.map(_.row).max)
    (topLeft, bottomRight)
  }

  lazy val kernel: CellUnit = {
    val unitPos = UnitPos(this, pivot)
    CellUnit(Command.all.toSet.flatMap { act: Action => GridOperations.transformUnitPos(unitPos, act).cells }, pivot)
  }
}
