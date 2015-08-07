package eu.shiftforward.icfpc2015

import eu.shiftforward.icfpc2015.model._

case class UnitPos(unit: CellUnit, pos: Cell) {
  def cells: List[Cell] = {
    val (x, y) = (pos.x - unit.pivot.x, pos.y - unit.pivot.y)

    unit.members.map { cell =>
      Cell(cell.x + x, cell.y + y)
    }
  }
}
