package eu.shiftforward.icfpc2015.solver

import eu.shiftforward.icfpc2015.GameState
import eu.shiftforward.icfpc2015.model.{ Grid, Input }
import spray.json._

import scala.io.Source
import scala.util.Random

case class OptimizationResult(score: Long, parameters: (Double, Double, Double, Double))

trait Optimizer {
  def score(filename: String, a: Double, b: Double, c: Double, d: Double) = {
    val input = Source.fromFile(filename).mkString.parseJson.convertTo[Input]
    val solver = new SmartSolver(a, b, c, d, debugOnGameOver = false)
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

object GeneticOptimizer extends Optimizer {
  class GeneticExploration[Gene, Specimen <% Iterable[Gene]](val mutationRate: Double,
                                                             val population: Int,
                                                             geneGenerator: () => Gene,
                                                             specimenBuilder: Iterable[Gene] => Specimen,
                                                             fitnessF: Specimen => Long,
                                                             stopCondition: (Int, List[Specimen]) => Boolean) {

    type Pool = List[Specimen]
    type MatePool = List[(Specimen, Long)]

    def newSpecimen(len: Int): Specimen = specimenBuilder(Stream.continually(geneGenerator()).take(len))

    def randomPool(archetype: Specimen): Pool = {
      val newPool = (1 to population).map(_ => newSpecimen(archetype.size)).toList
      newPool foreach println
      newPool
    }

    implicit def toMatePool(p: Pool): MatePool = matePool(p)

    def evolution(pool: MatePool, epoch: Int = 0): (MatePool, Int) = {
      println(s"Mating Epoch $epoch")

      val best = pool.maxBy(_._2)

      println(f"Epoch $epoch\tBest Fit ${best._2}%4f\tSpecimen ${best._1}")

      val newGeneration = popReproduction(pool)
      if (stopCondition(epoch, newGeneration)) (newGeneration, epoch)
      else evolution(newGeneration, epoch + 1)
    }

    def matePool(pool: Pool): MatePool = {
      val fitnesses = pool.map(fitnessF).toArray
      pool.zip(fitnesses)
    }

    def renormalize(vector: Array[Long]): Array[Double] = {
      val sum = vector.sum
      vector.map(_.toDouble / sum)
    }

    def popReproduction(matePool: MatePool): Pool = {
      val normalizedPool = matePool.map(_._1).zip(renormalize(matePool.map(_._2).toArray))
      (1 to population).par.map(_ =>
        crossover(monteCarlo(normalizedPool), monteCarlo(normalizedPool))).toList
    }

    def monteCarlo[A](weightedList: List[(A, Double)]): A =
      weightedList(Random.nextInt(weightedList.length)) match {
        case (s, f) if f > Random.nextFloat => s
        case _ => monteCarlo(weightedList)
      }

    def crossover(a: Specimen, b: Specimen): Specimen =
      mutate(specimenBuilder(a.zip(b).map(gene =>
        if (Random.nextFloat >= 0.5) gene._1 else gene._2)))

    def mutate(s: Specimen): Specimen =
      specimenBuilder(s.map(gene =>
        if (mutationRate > Random.nextFloat) geneGenerator() else gene))
  }

  def optimize(filename: String, maxIter: Int) = {
    type Specimen = (Double, Double, Double, Double)
    type Gene = Double

    def fitness(s: Specimen): Long = {
      val (a, b, c, d) = s
      score(filename, a, b, c, d).score
    }

    implicit def toIterable(s: Specimen): Iterable[Gene] = Array(s._1, s._2, s._3, s._4)

    val petri = new GeneticExploration[Gene, Specimen](
      0.1, 100, () => (Random.nextDouble() - 0.5) * 2, // rate of mutation, max population and gene pool
      cs => { val x = cs.toArray; (x(0), x(1), x(2), x(3)) }, // how to build a specimen from genes
      fitness, // the fitness function
      (iter, _) => iter > 10 // the stop condition
    )

    val best = petri.evolution(petri.toMatePool(petri.randomPool((1, 1, 1, 1))))._1.maxBy(_._2)

    println(f"DONE\tBest Fit ${best._2}%4f\tSpecimen ${best._1}")

    OptimizationResult(best._2, best._1)
  }
}

object OptimizerMain extends App {
  println(GeneticOptimizer.optimize(args(0), 20))
}