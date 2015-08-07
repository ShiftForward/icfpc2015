package eu.shiftforward.icfpc2015

import eu.shiftforward.icfpc2015.model._
import scala.io.Source
import spray.json._

object Interactive extends App with GridOperations {
  val input = Source.fromFile(args(0)).mkString.parseJson.convertTo[Input]

  val units = input.orderedUnitsBySeed(input.sourceSeeds.head)

  def loop(grid: Grid, currentUnitPos: Option[UnitPos]): Unit = {
    currentUnitPos match {
      case Some(pos) =>
        val state = GameStateRenderer.asString(grid, currentUnitPos)
        println(state)
        readLine("> ") match {
          case ":q" =>
          case ch => {
            val command = Command(ch(0))
            loop(grid, transform(pos, command, grid))
          }
        }

      case None =>
        loop(grid, initialPosition(units.next, grid))
    }
  }

  val grid = Grid(input.width, input.height).filled(input.filled: _*)
  loop(grid, None)
}
