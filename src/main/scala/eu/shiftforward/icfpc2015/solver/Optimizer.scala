package eu.shiftforward.icfpc2015.solver

import eu.shiftforward.icfpc2015.GameState
import eu.shiftforward.icfpc2015.model._
import spray.json._

import scala.io.Source
import scala.util.Random

case class OptimizationResult(score: Long, parameters: Array[Double])

trait Optimizer {
  val hyperparametersLenght = 42

  protected def score(filename: String, hp: Array[Double]) = {
    val input = Source.fromFile(filename).mkString.parseJson.convertTo[Input]
    val solver = new SmartSolver(hp, debugOnGameOver = false)

    val score = input.sourceSeeds.map { seed =>
      val units = input.orderedUnitsBySeed(seed)
      val grid = Grid(input.width, input.height).filled(input.filled: _*)
      val powerPhrases = PowerPhrase.knownPhrases

      val gameState = GameState(grid, units, powerPhrases)
      val solution = solver.play(gameState).toList
      gameState.nextState(solution).score.currentScore
    }.sum / input.sourceSeeds.size

    OptimizationResult(score, hp)
  }

  def optimize(filename: Array[String], maxIter: Int): OptimizationResult
}

object RandomOptimizer extends Optimizer {
  def optimize(filename: Array[String], maxIter: Int) = {
    def optimizeAux(iter: Int, bestModel: OptimizationResult): OptimizationResult = iter match {
      case 0 => bestModel
      case i =>
        val hp = Array.fill(hyperparametersLenght)((Random.nextDouble() - 0.5) * 2)
        val newModel = score(filename(0), hp)
        if (newModel.score > bestModel.score) optimizeAux(i - 1, newModel)
        else optimizeAux(i - 1, bestModel)
    }
    optimizeAux(maxIter, score(filename(0), Array.fill(hyperparametersLenght)(0.5)))
  }
}

object GeneticOptimizer extends Optimizer {
  type Specimen = Array[Double]
  type Gene = Double

  class GeneticExploration(mutationRate: Double,
                           crossOverRate: Double,
                           population: Int,
                           geneGenerator: () => Gene,
                           geneMutator: Gene => Gene,
                           specimenBuilder: Iterable[Gene] => Specimen,
                           specimenFixer: Specimen => Specimen,
                           fitnessF: Specimen => Long,
                           stopCondition: (Int, List[Specimen]) => Boolean)(implicit ev1: Specimen => Iterable[Gene]) {

    type Pool = List[Specimen]
    type MatePool = List[(Specimen, Long)]

    def newSpecimen(len: Int): Specimen = specimenBuilder(Stream.continually(geneGenerator()).take(len))

    def randomPool(archetype: Specimen, population: Int = population): Pool = {
      (1 to population).map(_ => newSpecimen(archetype.length)).toList
    }

    implicit def toMatePool(p: Pool): MatePool = matePool(p)

    def evolution(pool: MatePool, epoch: Int = 0): (MatePool, Int) = {
      val best = pool.maxBy(_._2)

      println(f"Epoch $epoch  Best Fit ${best._2}  Specimen ${best._1.toList}")

      val newGeneration = popReproduction(pool)
      if (stopCondition(epoch, newGeneration)) (newGeneration, epoch)
      else evolution(newGeneration, epoch + 1)
    }

    private[this] def matePool(pool: Pool): MatePool = {
      val fitnesses = pool.par.map(fitnessF).toArray
      pool.zip(fitnesses)
    }

    @inline private[this] def renormalize(vector: Array[Long]) = {
      val sum = vector.sum
      vector.map(_.toDouble / sum)
    }

    private[this] def popReproduction(matePool: MatePool): Pool = {
      val normalizedPool = matePool.map(_._1).zip(renormalize(matePool.map(_._2).toArray))

      // Always preserve the better specimen (elitist)
      (matePool.maxBy(_._2)._1 +:
        (1 until population).par.map(_ => crossover(monteCarlo(normalizedPool), monteCarlo(normalizedPool)))
      ).toList
    }

    private[this] def monteCarlo[A](weightedList: List[(A, Double)]): A =
      weightedList(Random.nextInt(weightedList.length)) match {
        case (s, f) if f > Random.nextFloat => s
        case _ => monteCarlo(weightedList)
      }

    private[this] def crossover(a: Specimen, b: Specimen): Specimen =
      mutate(specimenBuilder(a.zip(b).map(gene =>
        if (Random.nextFloat >= crossOverRate) gene._1 else gene._2)))

    private[this] def mutate(s: Specimen): Specimen =
      specimenBuilder(s.map(gene =>
        if (mutationRate > Random.nextFloat) geneMutator(gene) else gene))
  }

  def optimize(filenames: Array[String], maxIter: Int) = {
    def fitness(s: Specimen): Long = {
      filenames.map { filename =>
        score(filename, s).score
      }.sum
    }

    val population = 64

    val petri = new GeneticExploration(
      0.05, 0.5, population, // rate of mutation, crossover ratio, max population
      () => (Random.nextDouble() - 0.5) * 2, // random gene pool
      _ => (Random.nextDouble() - 0.5) * 2, // gene mutator
      cs => cs.map(v => (math rint v * 100) / 100).toArray, // how to build a specimen from genes
      cs => { val sum = cs.sum; cs.map(_ / sum) },
      fitness, // the fitness function
      (iter, _) => iter > 20 // the stop condition
    )

    // Preserve previous runs...
    val knowledgePool = List[Specimen](
      /* problem 0 (6125) */ Array(0.32, -0.16, 0.88, -0.42, -0.89, 0.21, 0.62, 0.36, 0.56, 0.95, 0.12, -0.9, 0.87, -0.18, 0.84, 0.06, 0.46, 0.26, -0.99, -0.71, -0.8, -0.85, -0.24, 0.02, 0.49, -0.05, 0.72, -0.49, -0.26, 0.58, 0.01, 0.66, 0.92, -0.82, 0.98, -0.75, 0.84, 0.88, -0.07, -0.78, 0.78, -0.79),
      /* problem 1 (2038) */ Array(0.51, 0.59, 0.36, -0.49, 0.97, -0.38, 0.6, 0.77, 0.8, 0.17, 0.33, -0.58, 0.31, -0.15, -0.39, 0.84, 0.74, 0.57, -0.75, 0.62, 0.81, -0.64, 0.79, -0.96, -0.11, -0.06, 0.54, -0.52, -0.06, 0.11, 0.73, 0.58, 0.75, 0.12, -0.15, -0.28, 0.24, -0.79, 0.8, -0.99, 0.49, 0.72),
      /* problem 2 (5435) */ Array(-0.78, -0.18, 1.0, -0.16, 0.85, 0.12, 0.32, -0.06, 0.49, 0.17, 0.68, 0.74, 0.62, 0.89, 0.8, -0.98, 0.33, 0.84, -0.48, 0.01, -0.34, -0.76, 0.01, 0.71, 0.02, -0.15, 0.47, -0.0, -0.58, -0.75, 0.18, 0.57, 0.1, 0.98, -0.26, 0.86, -0.92, -0.71, 0.1, 0.96, 0.96, 0.31),
      /* problem 3 (3235) */ Array(0.18, -0.16, 0.77, -0.21, -0.49, 0.9, 0.09, 0.36, -0.36, -0.23, 0.66, 0.53, -0.74, -0.08, 0.52, 0.65, -0.72, 0.44, -0.27, -0.94, -0.39, -0.65, 0.1, 0.02, -0.6, 0.13, 0.48, -0.43, -0.26, 0.58, -0.57, 0.41, 0.21, 0.52, -0.74, 0.64, -0.24, 0.76, 0.76, -0.95, 0.63, -0.98),
      /* problem 4 (3638) */ Array(0.21, -0.9, 0.74, 0.72, 0.12, 0.19, 0.7, 0.69, 0.81, 0.17, -0.39, -0.9, -0.11, -0.49, 0.35, -0.77, -0.02, 0.18, -0.51, -0.64, -0.48, -0.09, -0.24, 0.18, -0.69, -0.72, -0.22, 0.92, -0.06, -0.75, 0.64, -0.1, -0.29, -0.75, 0.65, -0.65, -0.23, -0.5, 0.09, -0.34, -0.19, -0.49),
      /* problem 5 (2577) */ Array(-0.8, -0.74, 0.6, -0.68, -0.39, 0.02, 0.84, -0.31, 0.2, 0.63, 0.37, 0.78, 0.39, 0.86, 0.39, 0.18, -0.17, 0.01, 0.99, 0.85, 0.81, 0.94, -0.31, 1.0, 0.0, 0.66, -0.66, -0.02, -0.01, 0.43, -0.13, 0.69, 0.55, -0.95, 0.25, -0.1, -0.21, -0.96, -0.27, 0.57, -0.52, 0.44)
    )

    val best = petri.evolution(petri.toMatePool(
      knowledgePool ++ petri.randomPool(Array.fill(hyperparametersLenght)(0.5), population - knowledgePool.length))
    )._1.maxBy(_._2)

    println(f"DONE\tBest Fit ${best._2}\tSpecimen ${best._1.toList}")

    OptimizationResult(best._2, best._1)
  }
}

object OptimizerMain extends App {
  println(GeneticOptimizer.optimize(args, 20))
}