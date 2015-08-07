package eu.shiftforward.icfpc2015.model

import eu.shiftforward.icfpc2015.Utils
import spray.json.DefaultJsonProtocol._

case class Grid(width: Int, height: Int, grid: Array[Array[Boolean]]) {

  val directions = Array(
    Array(
      Cell(1, 0), Cell(0, -1), Cell(-1, -1),
      Cell(-1, 0), Cell(-1, 1), Cell(0, 1)),
    Array(
      Cell(1, 0), Cell(1, -1), Cell(0, -1),
      Cell(-1, 0), Cell(0, 1), Cell(1, 1)))

  def neighbors(cell: Cell, direction: Int) = {
    val parity = cell.y & 1
    val dir = directions(parity)(direction)
    (cell.x + dir.x, cell.y + dir.y)
  }

  def isFilled(col: Int, row: Int): Boolean = grid(col)(row)

  def updatedInnerGrid(gridState: Array[Array[Boolean]], cell: Cell, value: Boolean) =
    gridState.updated(cell.col, gridState(cell.col).updated(cell.row, value))
  def filled(cells: Cell*) = copy(grid = cells.foldLeft(grid) { (oldGrid, cell) =>
    updatedInnerGrid(oldGrid, cell, value = true)
  })
  def removed(cells: Cell*) = copy(grid = cells.foldLeft(grid) { (oldGrid, cell) =>
    updatedInnerGrid(oldGrid, cell, value = false)
  })
}

object Grid {
  def apply(width: Int, height: Int): Grid = Grid(width, height, Array.ofDim[Boolean](width, height))
}

case class Cube(x: Int, y: Int, z: Int) {
  def rotate60R = Cube(-z, -x, -y)
  def rotate60L = Cube(-y, -z, -x)
}

case class Cell(x: Int, y: Int) {
  def col = x
  def row = y

  private[model] def cube = {
    val x = col - (row - (row & 1)) / 2
    val z = row
    val y = -x - z

    Cube(x, y, z)
  }

  def rotate60L(pivot: Cell) = {
    val pivotCube = pivot.cube
    val cellCube = this.cube

    val centered = Cube(
      cellCube.x - pivotCube.x, cellCube.y - pivotCube.y, cellCube.z - pivotCube.z).rotate60L

    Cell(
      Cube(centered.x + pivotCube.x, centered.y + pivotCube.y, centered.z + pivotCube.z))
  }

  def rotate60R(pivot: Cell) = {
    val pivotCube = pivot.cube
    val cellCube = this.cube

    val centered = Cube(
      cellCube.x - pivotCube.x, cellCube.y - pivotCube.y, cellCube.z - pivotCube.z).rotate60R

    Cell(
      Cube(centered.x + pivotCube.x, centered.y + pivotCube.y, centered.z + pivotCube.z))
  }
}

object Cell {
  def apply(cube: Cube): Cell = {
    val col = cube.x + (cube.z - (cube.z & 1)) / 2
    val row = cube.z

    Cell(col, row)
  }
}

case class CellUnit(members: List[Cell], pivot: Cell) {
  def rotate60L = CellUnit(members.map(_.rotate60L(pivot)), pivot)

  def rotate60R = CellUnit(members.map(_.rotate60R(pivot)), pivot)
}

case class Input(id: Int,
                 units: List[CellUnit],
                 width: Int,
                 height: Int,
                 filled: List[Cell],
                 sourceLength: Int,
                 sourceSeeds: List[Int]) {

  def orderedUnits(seed: Int) = Utils.random(seed).map(rnd => units(rnd.toInt % units.length))
}

object Input {
  implicit val cellJsonFormat = jsonFormat2(Cell.apply)
  implicit val cellUnitJsonFormat = jsonFormat2(CellUnit)
  implicit val inputJsonFormat = jsonFormat7(Input.apply)
}
