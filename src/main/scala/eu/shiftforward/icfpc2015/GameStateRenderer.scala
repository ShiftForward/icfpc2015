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

  def asString(grid: Grid, unit: Option[UnitPos] = None) = {

    def cellTypeOf(col: Int, row: Int): CellType = {
      val isFilled = grid.isFilled(col, row)
      val isPivot = unit.isDefined && unit.get.pos.col == col && unit.get.pos.row == row
      val isUnit = unit.exists(_.cells.contains(Cell(col, row)))
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
        (0 until grid.width).map { col => renderCell(col, row) }.mkString(" ")

    val header = "    " + (0 until grid.width).map { x => f"$x%02d" }.mkString("  ") + "\n"

    header + (0 until grid.height).map(rowString).mkString("\n") + "\n" + "  " + header
  }

  def stateAsString(state: GameState) = {
    val previousCommands = "Previous commands: " + state.commandHistory.map(_.ch).mkString

    val statsHeader =
      s"Stats\tH ${state.grid.aggHeight} L ${state.grid.aggLow} HL ${state.grid.highLow} CL ${state.grid.fullLines} HO ${state.grid.holes} BP ${state.grid.bumpiness} UN ${state.placedUnits}\n" +
        s"Current Score: ${state.score.fold("?")(_.currentScore.toString)}"

    statsHeader + "\n" + asString(state.grid, state.unitPosState.map(_.unitPos)) + "\n" + previousCommands
  }
}
