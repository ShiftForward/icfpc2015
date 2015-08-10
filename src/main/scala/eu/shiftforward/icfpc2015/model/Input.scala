package eu.shiftforward.icfpc2015.model

import eu.shiftforward.icfpc2015.util.Utils
import spray.json.DefaultJsonProtocol._

final case class Input(id: Int,
                       units: List[CellUnit],
                       width: Int,
                       height: Int,
                       filled: List[Cell],
                       sourceLength: Int,
                       sourceSeeds: List[Int]) {

  def orderedUnitsByGame(idx: Int) = orderedUnitsBySeed(sourceSeeds(idx))
  def orderedUnitsBySeed(seed: Int) =
    Utils.random(seed).map(rnd => units(rnd.toInt % units.length)).take(sourceLength).toList
}

object Input {
  implicit val cellJsonFormat = jsonFormat(Cell.apply, "x", "y")
  implicit val cellUnitJsonFormat = jsonFormat(CellUnit, "members", "pivot")
  implicit val inputJsonFormat = jsonFormat7(Input.apply)
}
