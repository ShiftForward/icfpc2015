package eu.shiftforward.icfpc2015.model

import spray.json.DefaultJsonProtocol._

case class Grid(width: Int, height: Int) {
  val grid = Array.ofDim[Boolean](width, height)

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

  def filled(col: Int, row: Int): Boolean = grid(col)(row)
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

  def rotate60L(pivot: Cell) = Cell(
    Cell(this.x - pivot.x, this.y - pivot.y).cube.rotate60L)

  def rotate60R(pivot: Cell) = Cell(
    Cell(this.x - pivot.x, this.y - pivot.y).cube.rotate60R)
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
                 sourceSeeds: List[Int])

object Input {
  implicit val cellJsonFormat = jsonFormat2(Cell.apply)
  implicit val cellUnitJsonFormat = jsonFormat2(CellUnit)
  implicit val inputJsonFormat = jsonFormat7(Input.apply)
}
