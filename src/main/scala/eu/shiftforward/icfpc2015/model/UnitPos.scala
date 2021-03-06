package eu.shiftforward.icfpc2015.model

case class UnitPos(unit: CellUnit, pos: Cell) {
  lazy val cells: Set[Cell] = {
    val cubedPos = pos.cube
    val cubedUnitPivot = unit.pivot.cube
    val (x, y, z) = (cubedPos.x - cubedUnitPivot.x, cubedPos.y - cubedUnitPivot.y, cubedPos.z - cubedUnitPivot.z)

    unit.members.map { cell =>
      val cubedCell = cell.cube
      Cell(cubedCell.copy(cubedCell.x + x, cubedCell.y + y, cubedCell.z + z))
    }
  }

  lazy val kernel: Set[Cell] =
    UnitPos(unit.kernel, pos).cells

  lazy val topRow: Int = {
    val (topLeft, _) = unit.boundingBox
    Math.max(0, pos.y - unit.pivot.row - topLeft.row)
  }
}
