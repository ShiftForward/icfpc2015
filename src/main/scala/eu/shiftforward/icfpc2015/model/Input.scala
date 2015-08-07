package eu.shiftforward.icfpc2015.model

import spray.json.DefaultJsonProtocol._

case class Cell(x: Int, y: Int)

case class CellUnit(members: List[Cell],
                    pivot: Cell)

case class Input(id: Int,
                 units: List[CellUnit],
                 width: Int,
                 height: Int,
                 filled: List[CellUnit],
                 sourceLength: Int,
                 sourceSeeds: List[Int])

object Input {
  implicit val cellJsonFormat = jsonFormat2(Cell)
  implicit val cellUnitJsonFormat = jsonFormat2(CellUnit)
  implicit val inputJsonFormat = jsonFormat7(Input.apply)
}
