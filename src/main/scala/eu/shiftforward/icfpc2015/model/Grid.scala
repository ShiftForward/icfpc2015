package eu.shiftforward.icfpc2015.model

import scala.collection.mutable

case class Grid(width: Int,
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

  def apply(width: Int, height: Int, grid: Array[Array[Boolean]],
            lineCount: Array[Int], colCount: Array[Int], heights: Array[Int]): Grid = {
    val (aggHeight, aggLow, bumpiness, holes, fullLines) = {
      var maxHeight = heights(0)
      var holesAcc = heights(0) - colCount(0)
      var minHeight = heights(0)
      var bumpAcc = 0
      var oldHeight = heights(0)
      var col = 1

      while (col < width) {
        val newHeight = heights(col)
        val newHoles = heights(col) - colCount(col)
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
