package eu.shiftforward.icfpc2015

import eu.shiftforward.icfpc2015.model._
import scala.io.Source
import spray.json._
import GridOperations._

object Interactive extends App {
  val input = Source.fromFile(args(0)).mkString.parseJson.convertTo[Input]

  val units: Iterator[CellUnit] = input.orderedUnitsBySeed(input.sourceSeeds.head)

  val grid = Grid(input.width, input.height).filled(input.filled: _*)

  def loop(state: GameState): Unit = {
    println(GameStateRenderer.asString(state))

    if (state.gameOver) println("GAME OVER")
    else readLine("> ") match {
      case ":q" => // Do Nothing
      case str =>
        loop(state.nextState(str))
    }
  }

  loop(GameState(grid, units))
}
