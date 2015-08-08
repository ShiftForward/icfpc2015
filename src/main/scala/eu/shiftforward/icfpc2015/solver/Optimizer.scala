package eu.shiftforward.icfpc2015.solver

import eu.shiftforward.icfpc2015.GameState
import eu.shiftforward.icfpc2015.model.{ Output, Grid, Input, Command }
import spray.json._

import scala.io.Source
import scala.util.Random

case class OptimizationResult(score: Long, paramenters: (Double, Double, Double, Double))

trait Optimizer {

  def score(filename: String, a: Double, b: Double, c: Double, d: Double) = {
    val input = Source.fromFile(filename).mkString.parseJson.convertTo[Input]
    val solver = new SmartSolver(a, b, c, d)
    val score = input.sourceSeeds.map { seed =>
      val units = input.orderedUnitsBySeed(seed)
      val grid = Grid(input.width, input.height).filled(input.filled: _*)
      val gameState = GameState(grid, units)
      val solution = solver.play(gameState).toList
      gameState.nextState(solution).score.currentScore
    }.sum / input.sourceSeeds.size
    OptimizationResult(score, (a, b, c, d))
  }

  def optimize(filename: String, maxIter: Int): OptimizationResult
}

object RandomOptimizer extends Optimizer {
  def optimize(filename: String, maxIter: Int) = {
    def optimizeAux(iter: Int, bestModel: OptimizationResult): OptimizationResult = iter match {
      case 0 => bestModel
      case i =>
        val (a, b, c, d) =
          (Random.nextDouble() - 0.5, Random.nextDouble() - 0.5, Random.nextDouble() - 0.5, Random.nextDouble() - 0.5)
        val newModel = score(filename, a, b, c, d)
        if (newModel.score > bestModel.score) optimizeAux(i - 1, newModel)
        else optimizeAux(i - 1, bestModel)
    }
    optimizeAux(maxIter, score(filename, 0, 0, 0, 0))
  }
}

object OptimizerMain extends App {
  println(RandomOptimizer.optimize(args(0), 20))
}