package eu.shiftforward.icfpc2015

import eu.shiftforward.icfpc2015.model._
import eu.shiftforward.icfpc2015.solver.SmartSolver
import spray.json._

import scala.io.Source

object Interactive extends App {
  val input = Source.fromFile(args(0)).mkString.parseJson.convertTo[Input]

  val units: Iterator[CellUnit] = input.orderedUnitsBySeed(input.sourceSeeds.head)

  val grid = Grid(input.width, input.height).filled(input.filled: _*)

  def loop(state: GameState): Unit = {
    println(s"Stats\tH ${state.grid.aggHeight} CL ${state.grid.fullLines} HO ${state.grid.holes} BP ${state.grid.bumpiness}")
    println(s"Current Score: ${state.score.currentScore}")

    println(GameStateRenderer.asString(state.grid, state.currentUnitPos))

    if (state.gameOver) println("GAME OVER")
    else readLine("> ") match {
      case ":q" => // Do Nothing
      case ":fit" =>
        println("Possible fits of current piece (with rotations): ")

        val tgts = SmartSolver.possibleTargets(state)
        println(GameStateRenderer.asString(state.grid.filled(tgts.flatMap(_.cells): _*)))
        loop(state)
      case str =>
        loop(state.nextState(str))
    }
  }

  loop(GameState(grid, units))
}
