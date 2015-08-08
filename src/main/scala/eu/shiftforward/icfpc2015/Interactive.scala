package eu.shiftforward.icfpc2015

import eu.shiftforward.icfpc2015.GameState.UnitPosState
import eu.shiftforward.icfpc2015.GridOperations._
import eu.shiftforward.icfpc2015.model._
import eu.shiftforward.icfpc2015.solver.SmartSolver
import spray.json._

import scala.io.Source
import scala.util.Try

object Interactive extends App {
  val input = Source.fromFile(args(0)).mkString.parseJson.convertTo[Input]

  val units = input.orderedUnitsBySeed(input.sourceSeeds.head)

  val grid = Grid(input.width, input.height).filled(input.filled: _*)

  def loop(state: GameState): Unit = try {
    println(GameStateRenderer.stateAsString(state))

    if (state.gameOver) println("GAME OVER")
    else readLine("> ") match {
      case ":q" => // Quit

      case str if str.startsWith(":skip") =>
        val split = str.split(" ")
        val size = Try(split(1).toInt).getOrElse(1)
        val stateUnits = state.units.drop(size - 1)
        initialPosition(stateUnits.head, state.grid) match {
          case Some(newUnit) =>
            loop(state.copy(
              unitPosState = Some(UnitPosState(newUnit, Set(newUnit))),
              units = stateUnits.tail))
          case None => loop(state)
        }

      case ":fit" =>
        println("Possible fits of current piece (with rotations): ")
        val tgts = SmartSolver.possibleTargets(state)
        println(GameStateRenderer.asString(state.grid.filled(tgts.flatMap(_.cells): _*)))
        loop(state)

      case str if str.startsWith(":path") =>
        // FIXME: This builds a path to the desired cell without rotating the unit
        val split = str.split(" ")
        val pos = Cell(split(1).toInt, split(2).toInt)
        val execute = split.size == 4
        val unit = state.currentUnitPos.get.unit
        val path = SmartSolver.findPath(state, UnitPos(unit, pos))
        path match {
          case Some(l) =>
            val pathStr = l.map(_.ch).mkString
            println(s"Path to (${pos.x}, ${pos.y}): $pathStr")
            loop(if (execute) state.nextState(pathStr) else state)
          case None =>
            println(s"No path to (${pos.x}, ${pos.y})")
            loop(state)
        }

      case str =>
        loop(state.nextState(str))
    }
  } catch {
    case t: Throwable =>
      t.printStackTrace()
      loop(state)
  }

  loop(GameState(grid, units))
}
