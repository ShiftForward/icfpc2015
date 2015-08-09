package eu.shiftforward.icfpc2015

import eu.shiftforward.icfpc2015.model.Input._
import eu.shiftforward.icfpc2015.model._
import eu.shiftforward.icfpc2015.solver.{ SmartSolver, Solver }
import spray.json._

import scala.io.Source

object SolverDebugger extends App {
  val solver: Solver = new SmartSolver

  val solutionTag: Option[String] = None

  val input = Source.fromFile(args(0)).mkString.parseJson.convertTo[Input]

  val startTime = System.currentTimeMillis()

  input.sourceSeeds.map { seed =>
    val units = input.orderedUnitsBySeed(seed)
    val grid = Grid(input.width, input.height).filled(input.filled: _*)
    val powerPhrases = PowerPhrase.knownPhrases

    val solution = solver.play(GameState(grid, units, powerPhrases, countScore = false)).toList

    println(s"Solution with seed $seed: ${solution.map(_.ch).mkString}")
    Output(input.id, seed, solutionTag, solution)
  }

  val endTime = System.currentTimeMillis()

  println("Results obtained in " + (endTime - startTime) + " ms")

}