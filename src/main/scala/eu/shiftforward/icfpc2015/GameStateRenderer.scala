package eu.shiftforward.icfpc2015

import eu.shiftforward.icfpc2015.model._

object GameStateRenderer {
  sealed trait CellType {
    def icon: String
  }

  case object EmptyCell extends CellType { val icon = " \u2B21 " }
  case object FilledCell extends CellType { val icon = " \u2B22 " }
  case object EmptyPivotCell extends CellType { val icon = "'\u2B21'" }
  case object FilledPivotCell extends CellType { val icon = "'\u2B22'" }
  case object UnitPivotCell extends CellType { val icon = "'\u2B24'" }
  case object UnitCell extends CellType { val icon = " \u2B24 " }
  case object UnknownCell extends CellType { val icon = "⟨?⟩" }

  def asString(grid: Grid, unit: Option[UnitPos] = None) = {

    def cellTypeOf(col: Int, row: Int): CellType = {
      val isFilled = grid.isFilled(col, row)
      val isPivot = unit.isDefined && unit.get.pos.col == col && unit.get.pos.row == row
      val isUnit = unit.isDefined && unit.get.cells.contains(Cell(col, row))
      (isPivot, isFilled, isUnit) match {
        case (false, false, false) => EmptyCell
        case (true, false, false) => EmptyPivotCell
        case (true, true, false) => FilledPivotCell
        case (true, false, true) => UnitPivotCell
        case (false, true, false) => FilledCell
        case (false, false, true) => UnitCell
        case _ => UnknownCell
      }
    }

    def renderCell(col: Int, row: Int) = cellTypeOf(col, row).icon
    def rowString(row: Int) =
      (if (row % 2 == 1) f"$row%02d:   " else f"$row%02d: ") +
        (0 until grid.width).map { col => renderCell(col, row) }.mkString(" ")

    val header = "    " + (0 until grid.width).map { x => f"$x%02d" }.mkString("  ") + "\n"

    "\n" + header + (0 until grid.height).map(rowString).mkString("\n") + "\n" + "  " + header
    header + (0 until grid.height).map(rowString).mkString("\n") + "\n" + "  " + header
  }
}

object GameStateRendererTest extends App {
  println(GameStateRenderer.asString(Grid(10, 15).filled(Cell(2, 2), Cell(2, 3)), Some(UnitPos(CellUnit(Set(Cell(0, 0)), Cell(0, 1)), Cell(9, 1)))))
}
