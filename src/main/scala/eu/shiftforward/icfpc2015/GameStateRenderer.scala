package eu.shiftforward.icfpc2015

import eu.shiftforward.icfpc2015.model._

object GameStateRenderer {
  sealed trait CellType {
    def icon: String
  }

  case object EmptyCell extends CellType { val icon = "⟨ ⟩" }
  case object FilledCell extends CellType { val icon = "⟨X⟩" }
  case object EmptyPivotCell extends CellType { val icon = "⟨.⟩" }
  case object FilledPivotCell extends CellType { val icon = "⟨/⟩" }
  case object UnitPivotCell extends CellType { val icon = "⟨=⟩" }
  case object UnitCell extends CellType { val icon = "⟨-⟩" }
  case object UnknownCell extends CellType { val icon = "⟨?⟩" }

  def asString(state: GameState) = {

    def cellTypeOf(col: Int, row: Int): CellType = {
      val isFilled = state.grid.isFilled(col, row)
      val isPivot = state.currentUnitPos.isDefined && state.currentUnitPos.get.pos.col == col && state.currentUnitPos.get.pos.row == row
      val isUnit = state.currentUnitPos.isDefined && state.currentUnitPos.get.cells.contains(Cell(col, row))
      (isPivot, isFilled, isUnit) match {
        case (false, false, false) => EmptyCell
        case (true, false, false) => EmptyPivotCell
        case (true, true, false) => FilledPivotCell
        case (true, false, true) => UnitPivotCell
        case (false, true, false) => FilledCell
        case (false, false, true) => UnitCell
        case _ => {
          println(s"Got an unknown cell! isPivot: $isPivot; isFilled: $isFilled; isUnit: $isUnit")
          UnknownCell
        }
      }
    }

    def renderCell(col: Int, row: Int) = cellTypeOf(col, row).icon
    def rowString(row: Int) =
      (if (row % 2 == 1) f"$row%02d:   " else f"$row%02d: ") +
        (0 until state.grid.width).map { col => renderCell(col, row) }.mkString(" ")

    val statsHeader =
      s"Stats\tH ${state.grid.aggHeight} CL ${state.grid.fullLines} HO ${state.grid.holes} BP ${state.grid.bumpiness}\n" +
        s"Current Score: ${state.score.currentScore}"

    val header = "    " + (0 until state.grid.width).map { x => f"$x%02d" }.mkString("  ") + "\n"

    statsHeader + "\n" + header + (0 until state.grid.height).map(rowString).mkString("\n") + "\n" + "  " + header
  }
}
