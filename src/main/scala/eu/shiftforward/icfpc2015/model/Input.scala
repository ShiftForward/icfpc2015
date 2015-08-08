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

  lazy val heights: Seq[Int] = (0 until width).map { c =>
    val h = column(c).indexWhere(identity)
    if (h < 0) 0 else height - h
  }

  lazy val (aggHeight, bumpiness) = {
    val loopResults = heights.tail.foldLeft((heights.head, 0, heights.head)) {
      case ((maxHeight, bump, oldHeight), newHeight) =>
        (math.max(newHeight, maxHeight), bump + math.abs(newHeight - oldHeight), newHeight)
    }
    (loopResults._1, loopResults._2)
  }

  lazy val fullLines: Int = (0 until height).count { r => row(r).forall(identity) }
  lazy val holes: Int = (0 until width).map { c => column(c).dropWhile(p => !p).count(p => !p) }.sum

  def row(r: Int): Array[Boolean] = grid(r)
  def column(c: Int): Array[Boolean] = grid.map(_(c))

  def isFilled(col: Int, row: Int): Boolean = grid(row)(col)

  def updatedInnerGrid(gridState: Array[Array[Boolean]], cell: Cell, value: Boolean) =
    gridState.updated(cell.row, gridState(cell.row).updated(cell.col, value))

  def filled(cells: Cell*) = copy(grid = cells.foldLeft(grid) { (oldGrid, cell) =>
    updatedInnerGrid(oldGrid, cell, value = true)
  })

  def removed(cells: Cell*) = copy(grid = cells.foldLeft(grid) { (oldGrid, cell) =>
    updatedInnerGrid(oldGrid, cell, value = false)
  })
}

object Grid {
  def apply(width: Int, height: Int): Grid = Grid(width, height, Array.ofDim[Boolean](height, width))
}

case class Cube(x: Int, y: Int, z: Int) {
  def rotateCW = Cube(-z, -x, -y)
  def rotateCCW = Cube(-y, -z, -x)
  def distance(that: Cube) =
    (math.abs(this.x - that.x) + math.abs(this.y - that.y) + math.abs(this.z - that.z)) / 2
}

case class Cell(x: Int, y: Int) {
  def col = x
  def row = y

  def cube = {
    val x = col - (row - (row & 1)) / 2
    val z = row
    val y = -x - z

    Cube(x, y, z)
  }

  def rotateCCW(pivot: Cell) = {
    val pivotCube = pivot.cube
    val cellCube = this.cube

    val centered = Cube(
      cellCube.x - pivotCube.x, cellCube.y - pivotCube.y, cellCube.z - pivotCube.z).rotateCCW

    Cell(
      Cube(centered.x + pivotCube.x, centered.y + pivotCube.y, centered.z + pivotCube.z))
  }

  def rotateCW(pivot: Cell) = {
    val pivotCube = pivot.cube
    val cellCube = this.cube

    val centered = Cube(
      cellCube.x - pivotCube.x, cellCube.y - pivotCube.y, cellCube.z - pivotCube.z).rotateCW

    Cell(
      Cube(centered.x + pivotCube.x, centered.y + pivotCube.y, centered.z + pivotCube.z))
  }

  def distance(that: Cell) =
    this.cube.distance(that.cube)
}

object Cell {
  def apply(cube: Cube): Cell = {
    val col = cube.x + (cube.z - (cube.z & 1)) / 2
    val row = cube.z

    Cell(col, row)
  }
}

case class CellUnit(members: Set[Cell], pivot: Cell) {
  def rotateCCW = CellUnit(members.map(_.rotateCCW(pivot)), pivot)

  def rotateCW = CellUnit(members.map(_.rotateCW(pivot)), pivot)

  def boundingBox: (Cell, Cell) = {
    val topLeft = Cell(members.map(_.col).min, members.map(_.row).min)
    val bottomRight = Cell(members.map(_.col).max, members.map(_.row).max)
    (topLeft, bottomRight)
  }
}

case class Input(id: Int,
                 units: List[CellUnit],
                 width: Int,
                 height: Int,
                 filled: List[Cell],
                 sourceLength: Int,
                 sourceSeeds: List[Int]) {

  def orderedUnitsByGame(idx: Int) = orderedUnitsBySeed(sourceSeeds(idx))
  def orderedUnitsBySeed(seed: Int) = Utils.random(seed).map(rnd => units(rnd.toInt % units.length)).take(sourceLength).toList
}

object Input {
  implicit val cellJsonFormat = jsonFormat2(Cell.apply)
  implicit val cellUnitJsonFormat = jsonFormat2(CellUnit)
  implicit val inputJsonFormat = jsonFormat7(Input.apply)
}
