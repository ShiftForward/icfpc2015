package eu.shiftforward.icfpc2015

import eu.shiftforward.icfpc2015.model._

object GameStateRenderer {
  def asString(grid: Grid) = {
    def renderCell(filled: Boolean) = filled match {
      case true => "⟨X⟩"
      case false => "⟨ ⟩"
    }
    def rowString(row: Int) =
      (if (row % 2 == 1) f"$row%02d:   " else f"$row%02d: ") +
        (0 until grid.width).map { col => renderCell(grid.isFilled(col, row)) }.mkString(" ")

    val header = "    " + (0 until grid.width).map { x => f"$x%02d" }.mkString("  ") + "\n"

    header + (0 until grid.height).map(rowString).mkString("\n") + "\n" + "  " + header
  }
}

object GameStateRendererTest extends App {
  println(GameStateRenderer.asString(Grid(10, 15).filled(Cell(2, 2), Cell(2, 3))))
}