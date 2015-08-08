package eu.shiftforward.icfpc2015

import java.io._

import eu.shiftforward.icfpc2015.GameState.UnitPosState
import eu.shiftforward.icfpc2015.GridOperations._
import eu.shiftforward.icfpc2015.model._
import eu.shiftforward.icfpc2015.solver._
import spray.json._

import scala.io.{ StdIn, Source }
import scala.util.Try

object Interactive extends App {
  val input = Source.fromFile(args(0)).mkString.parseJson.convertTo[Input]

  val units = input.orderedUnitsBySeed(input.sourceSeeds.head)

  val grid = Grid(input.width, input.height).filled(input.filled: _*)

  val solver = new SmartSolver // TODO refactor this

  def clearPreviousLines(lineCount: Int): Unit = {
    print(String.format("\033[%sA", (lineCount + 2).toString))
    print(String.format("\033[2J"))
  }

  val upString = new String(Array[Byte](27, 91, 65))
  val downString = new String(Array[Byte](27, 91, 66))
  val leftString = new String(Array[Byte](27, 91, 67))
  val rightString = new String(Array[Byte](27, 91, 68))

  def loop(state: GameState): Unit = try {
    val gameStateString = GameStateRenderer.stateAsString(state)
    clearPreviousLines(1000)
    println(gameStateString)

    if (state.gameOver) println("GAME OVER")
    else StdIn.readLine("> ") match {
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

        val tgts = solver.possibleTargets(state)
        val gameStateWithFitsString = GameStateRenderer.asString(state.grid.filled(tgts.flatMap(_.cells): _*))

        clearPreviousLines(gameStateString.split("\n").length - 2)
        println(gameStateWithFitsString)
        println("<press enter to continue>")
        StdIn.readLine()
        loop(state)

      case str if str.startsWith(":path") =>
        // FIXME: This builds a path to the desired cell without rotating the unit
        val split = str.split(" ")
        val pos = Cell(split(1).toInt, split(2).toInt)
        val execute = split.size == 4
        val unit = state.currentUnitPos.get.unit
        val pathFinder = new PathFinder(state.grid, state.currentUnitPos.get)
        val path = pathFinder.pathTo(UnitPos(unit, pos))
        path match {
          case Some(l) =>
            val pathStr = l.map(_.ch).mkString
            println(s"Path to (${pos.x}, ${pos.y}): $pathStr")
            loop(if (execute) state.nextState(pathStr) else state)
          case None =>
            println(s"No path to (${pos.x}, ${pos.y})")
            loop(state)
        }

      case s if s == upString =>
        loop(state.nextState(Command.mappings(MoveNW).head))
      case s if s == downString =>
        loop(state.nextState(Command.mappings(MoveSE).head))
      case s if s == leftString =>
        loop(state.nextState(Command.mappings(MoveE).head))
      case s if s == rightString =>
        loop(state.nextState(Command.mappings(MoveW).head))
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
