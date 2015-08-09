package eu.shiftforward.icfpc2015.model

import eu.shiftforward.icfpc2015._
import spray.json.DefaultJsonProtocol._

import scala.collection.mutable

/* final class BitSet2DArray(width: Int, height: Int) {
  var grid = Array.fill(height) { mutable.BitSet() }

  def apply(x: Int, y: Int) = grid(y)(x)
  def set(x: Int, y: Int) = grid(y) += x
  def clone(): BitSet2DArray = {
    val newGrid = new BitSet2DArray(width, height)
    newGrid.grid = grid.clone()
  }
} */

case class Grid(
    width: Int,
    height: Int,
    grid: Array[Array[Boolean]],
    lineCount: Array[Int],
    colCount: Array[Int],
    heights: Array[Int],
    aggHeight: Int,
    aggLow: Int,
    bumpiness: Int,
    holes: Int,
    fullLines: Int) {
  lazy val highLow = aggHeight - aggLow

  private[this] def column(c: Int): Array[Boolean] = grid.map(_(c))
  def isFilled(col: Int, row: Int): Boolean = grid(row)(col)

  def filled(cells: Cell*) = {
    val newGrid = grid.map(identity)
    val newLineCount = lineCount.clone()
    val newColCount = colCount.clone()
    val newHeights = heights.clone()
    var newAggHeight = aggHeight
    var newAggLow = aggLow
    var newBumpiness = bumpiness
    var newHoles = holes
    var newFullLines = fullLines
    var reviewAggLow = false

    val cloned = mutable.BitSet()

    cells.foreach { cell =>
      if (!cloned(cell.row)) {
        newGrid(cell.row) = newGrid(cell.row).clone()
        cloned += cell.row
      }

      if (!newGrid(cell.row)(cell.col)) {
        newGrid(cell.row)(cell.col) = true

        val prevColCount = newColCount(cell.col)
        val prevLineCount = newLineCount(cell.row)
        val prevHeight = newHeights(cell.col)

        newLineCount(cell.row) += 1
        newColCount(cell.col) += 1

        if (prevLineCount != width && newLineCount(cell.row) == width)
          newFullLines += 1

        if ((height - cell.row) > newHeights(cell.col)) {
          newHeights(cell.col) = height - cell.row

          if (prevHeight == newAggLow) {
            reviewAggLow = true
          }

          if (newHeights(cell.col) > newAggHeight)
            newAggHeight = newHeights(cell.col)

          if (cell.col > 0) {
            newBumpiness -= math.abs(prevHeight - newHeights(cell.col - 1))
            newBumpiness += math.abs(newHeights(cell.col) - newHeights(cell.col - 1))
          }
          if (cell.col < width - 1) {
            newBumpiness -= math.abs(prevHeight - newHeights(cell.col + 1))
            newBumpiness += math.abs(newHeights(cell.col) - newHeights(cell.col + 1))
          }
        }

        newHoles -= (prevHeight - prevColCount)
        newHoles += (newHeights(cell.col) - newColCount(cell.col))
      }
    }

    if (reviewAggLow)
      newAggLow = newHeights.min

    copy(
      grid = newGrid,
      lineCount = newLineCount,
      colCount = newColCount,
      heights = newHeights,
      aggHeight = newAggHeight,
      aggLow = newAggLow,
      bumpiness = newBumpiness,
      holes = newHoles,
      fullLines = newFullLines)
  }
}

object Grid {
  def apply(width: Int, height: Int): Grid = Grid(width, height, Array.ofDim[Boolean](height, width))
  def apply(width: Int, height: Int, grid: Array[Array[Boolean]]): Grid =
    Grid(
      width,
      height,
      grid,
      lineCount = grid.map(_.count(identity)),
      colCount = (0 until width).map { col =>
        grid.map(_(col)).count(identity)
      }.toArray,
      heights = (0 until width).map { col =>
        val h = grid.indexWhere(_(col))
        if (h < 0) 0 else height - h
      }.toArray)
  def apply(width: Int, height: Int, grid: Array[Array[Boolean]], lineCount: Array[Int], colCount: Array[Int], heights: Array[Int]): Grid = {
    val (aggHeight, aggLow, bumpiness, holes, fullLines) = {
      var maxHeight = heights(0)
      var holesAcc = (heights(0) - colCount(0))
      var minHeight = heights(0)
      var bumpAcc = 0
      var oldHeight = heights(0)
      var col = 1

      while (col < width) {
        val newHeight = heights(col)
        val newHoles = (heights(col) - colCount(col))
        maxHeight = math.max(newHeight, maxHeight)
        minHeight = math.min(newHeight, minHeight)
        bumpAcc += math.abs(newHeight - oldHeight)
        holesAcc += newHoles
        oldHeight = newHeight
        col += 1
      }

      (maxHeight, minHeight, bumpAcc, holesAcc, lineCount.count(_ == width))
    }

    Grid(width, height, grid, lineCount, colCount, heights, aggHeight, aggLow, bumpiness, holes, fullLines)
  }
}

final case class Cube(x: Int, y: Int, z: Int) {
  lazy val rotateCW = Cube(-z, -x, -y)
  lazy val rotateCCW = Cube(-y, -z, -x)
  def distance(that: Cube) =
    (math.abs(this.x - that.x) + math.abs(this.y - that.y) + math.abs(this.z - that.z)) / 2
}

final case class Cell(x: Int, y: Int) {
  @inline def col = x
  @inline def row = y

  lazy val cube = {
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
  lazy val rotateCCW = new CellUnit(members.map(_.rotateCCW(pivot)), pivot)

  lazy val rotateCW = new CellUnit(members.map(_.rotateCW(pivot)), pivot)

  lazy val boundingBox: (Cell, Cell) = {
    val topLeft = Cell(members.map(_.col).min, members.map(_.row).min)
    val bottomRight = Cell(members.map(_.col).max, members.map(_.row).max)
    (topLeft, bottomRight)
  }

  lazy val kernel: CellUnit = {
    val unitPos = UnitPos(this, pivot)
    CellUnit(Command.all.toSet.flatMap { act: Action => GridOperations.transformUnitPos(unitPos, act).cells }, pivot)
  }
}

final case class Input(id: Int,
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
  implicit val cellJsonFormat = jsonFormat(Cell.apply, "x", "y")
  implicit val cellUnitJsonFormat = jsonFormat(CellUnit, "members", "pivot")
  implicit val inputJsonFormat = jsonFormat7(Input.apply)
}
