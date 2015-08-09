package eu.shiftforward.icfpc2015

import eu.shiftforward.icfpc2015.model._

object GridOperations {

  def transform(unitPos: UnitPos, command: Command, grid: Grid): Option[UnitPos] = {
    val transformed = transformUnitPos(unitPos, command)

    if (fits(transformed, grid)) Some(transformed)
    else None
  }

  def lockCell(unitPos: UnitPos, grid: Grid): Grid =
    grid.filled(unitPos.cells.toList: _*)

  def removeLines(grid: Grid): (Grid, Int) = {
    val linesToRemove = grid.lineCount.count(_ == grid.width)
    if (linesToRemove == 0)
      (grid, 0)
    else {
      val removedGrid = grid.grid.zipWithIndex.filter { case (r, i) => grid.lineCount(i) != grid.width }.map(_._1)
      (Grid(grid.width, grid.height, Array.ofDim[Boolean](linesToRemove, grid.width) ++ removedGrid), linesToRemove)
    }
  }

  def initialPosition(unitPos: CellUnit, grid: Grid): Option[UnitPos] = {
    val (topLeft, bottomRight) = unitPos.boundingBox
    val unitWidth = bottomRight.col - topLeft.col + 1
    val leftShift = Math.floor((grid.width - unitWidth) / 2).toInt
    val x = Math.max(0, leftShift - topLeft.col + unitPos.pivot.col)
    // TODO: This might be wrong
    val y = Math.max(0, unitPos.pivot.row - topLeft.row)
    val initialCellUnit = UnitPos(unitPos, Cell(x, y))

    if (fits(initialCellUnit, grid)) Some(initialCellUnit)
    else None
  }

  def cellFits(cell: Cell, grid: Grid): Boolean = cell match {
    case Cell(x, y) =>
      x >= 0 && x < grid.width &&
        y >= 0 && y < grid.height &&
        !grid.isFilled(x, y)
  }

  def fits(unitPos: UnitPos, grid: Grid): Boolean = {
    unitPos.cells.forall { cell => cellFits(cell, grid) }
  }

  def transformUnitPos(unitPos: UnitPos, command: Command): UnitPos = transformUnitPos(unitPos, command.action)

  def transformUnitPos(unitPos: UnitPos, action: Action): UnitPos = action match {
    case MoveW => unitPos.copy(pos = unitPos.pos.copy(x = unitPos.pos.x - 1))
    case MoveE => unitPos.copy(pos = unitPos.pos.copy(x = unitPos.pos.x + 1))
    case MoveSW => unitPos.copy(pos = unitPos.pos.copy(x = unitPos.pos.x - (if (unitPos.pos.y % 2 == 0) 1 else 0), y = unitPos.pos.y + 1))
    case MoveSE => unitPos.copy(pos = unitPos.pos.copy(x = unitPos.pos.x + (if (unitPos.pos.y % 2 == 0) 0 else 1), y = unitPos.pos.y + 1))
    case MoveNW => unitPos.copy(pos = unitPos.pos.copy(x = unitPos.pos.x - (if (unitPos.pos.y % 2 == 0) 1 else 0), y = unitPos.pos.y - 1))
    case MoveNE => unitPos.copy(pos = unitPos.pos.copy(x = unitPos.pos.x + (if (unitPos.pos.y % 2 == 0) 0 else 1), y = unitPos.pos.y - 1))
    case RotateCW => unitPos.copy(unit = unitPos.unit.rotateCW)
    case RotateCCW => unitPos.copy(unit = unitPos.unit.rotateCCW)
  }

  def transformUnitPos(unitPos: UnitPos, transform: Transform): UnitPos = {
    unitPos.copy(
      pos = unitPos.pos.copy(
        x = unitPos.pos.x + (if (unitPos.pos.y % 2 == 0) transform.dx1 else transform.dx2),
        y = unitPos.pos.y + transform.dy),
      unit = (1 to transform.dr).foldLeft(unitPos.unit) { (unit, _) => unit.rotateCW })
  }

  case class Transform(dx1: Int, dx2: Int, dy: Int, dr: Int)

  def compileTransform(actions: Seq[Action]) = {
    var dx1, dx2, dy, dr = 0
    var parity = true

    actions.foreach {
      case MoveW =>
        dx1 -= 1; dx2 -= 1
      case MoveE =>
        dx1 += 1; dx2 += 1
      case MoveSW =>
        if (parity) dx1 -= 1 else dx2 -= 1; dy += 1; parity = !parity
      case MoveSE =>
        if (parity) dx2 += 1 else dx1 += 1; dy += 1; parity = !parity
      case MoveNW =>
        if (parity) dx1 -= 1 else dx2 -= 1; dy -= 1; parity = !parity
      case MoveNE =>
        if (parity) dx2 += 1 else dx1 += 1; dy -= 1; parity = !parity
      case RotateCW => dr = (dr + 1) % 6
      case RotateCCW => dr = (dr + 6 - 1) % 6
    }

    Transform(dx1, dx2, dy, dr)
  }
}
