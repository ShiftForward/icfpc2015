package eu.shiftforward.icfpc2015

import eu.shiftforward.icfpc2015.model._
import scala.io.Source
import spray.json._

object Interactive extends App with GridOperations {
  val input = Source.fromFile(args(0)).mkString.parseJson.convertTo[Input]

  val units = input.orderedUnitsBySeed(input.sourceSeeds.head)

  def loop(grid: Grid, currentUnitPos: Option[UnitPos]): Unit = {
    val state = GameStateRenderer.asString(grid, currentUnitPos)
    println(state)
    currentUnitPos match {
      case Some(pos) =>
        readLine("> ") match {
          case ":q" =>
          case ch => {
            val command = Command(ch(0))
            transform(pos, command, grid) match {
              case None => {
                val nextGrid = lockCell(pos, grid)
                loop(nextGrid, initialPosition(units.next, nextGrid))
              }
              case nextPos => loop(grid, nextPos)
            }
          }
        }

      case None =>
        println("GAME OVER!")
    }
  }

  val grid = Grid(input.width, input.height).filled(input.filled: _*)
  loop(grid, initialPosition(units.next, grid))
}
