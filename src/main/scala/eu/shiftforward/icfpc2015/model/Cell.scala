package eu.shiftforward.icfpc2015.model

import eu.shiftforward.icfpc2015.model.Cell._

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

  final case class Cube(x: Int, y: Int, z: Int) {
    lazy val rotateCW = Cube(-z, -x, -y)
    lazy val rotateCCW = Cube(-y, -z, -x)
    def distance(that: Cube) =
      (math.abs(this.x - that.x) + math.abs(this.y - that.y) + math.abs(this.z - that.z)) / 2
  }

  def apply(cube: Cube): Cell = {
    val col = cube.x + (cube.z - (cube.z & 1)) / 2
    val row = cube.z

    Cell(col, row)
  }
}
