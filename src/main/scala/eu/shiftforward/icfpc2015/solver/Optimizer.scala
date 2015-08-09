package eu.shiftforward.icfpc2015.solver

import eu.shiftforward.icfpc2015.GameState
import eu.shiftforward.icfpc2015.model._
import spray.json._

import scala.io.Source
import scala.util.Random

case class OptimizationResult(score: Long, parameters: Array[Double])

trait Optimizer {
  val hyperparametersLenght = 6 * 8

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

    val population = 128

    val petri = new GeneticExploration(
      0.05, 0.5, population, // rate of mutation, crossover ratio, max population
      () => (Random.nextDouble() - 0.5) * 2, // random gene pool
      _ => (Random.nextDouble() - 0.5) * 2, // gene mutator
      cs => cs.map(v => (math rint v * 100) / 100).toArray, // how to build a specimen from genes
      cs => { val sum = cs.sum; cs.map(_ / sum) },
      fitness, // the fitness function
      (iter, _) => iter > maxIter // the stop condition
    )

    // Preserve previous runs...
    val knowledgePool = List[Specimen](
      /* problem  0  (6733) */ Array(
        0.15, 0.9, 0.26, -0.21, 0.89, 0.06,
        0.3, 0.91, 0.97, 0.54, -0.18, 0.54,
        -0.72, -0.1, 0.06, 0.87, -0.8, 0.34,
        -0.62, 0.2, -0.3, -0.46, -0.95, 0.75,
        -0.84, 0.55, -0.32, 0.64, -0.9, 0.2,
        0.36, 0.97, -0.77, -0.73, -0.69, 0.93,
        -0.49, 0.38, 0.4, -0.99, 0.88, -0.23,
        0.59, 0.22, 0.94, -0.99, 0.2, -0.27),
      /* problem  1  (3544) */ Array(
        0.13, -0.5, 0.15, 0.69, 0.94, 0.51,
        -0.95, -0.95, 0.94, -0.89, -0.25, -0.01,
        -0.47, 0.41, -0.31, -0.33, -0.74, 0.51,
        0.61, 0.8, 0.85, -0.62, 0.25, 0.82,
        0.85, 0.49, 0.95, -0.74, 0.68, 0.22,
        -0.77, -0.05, -0.95, 0.45, -0.4, -0.47,
        0.6, 0.39, -0.27, -0.99, 0.41, -0.69,
        0, 0, 0, 0, 0, 0),
      /* problem  2  (6542) */ Array(
        -0.23, -0.5, 0.09, -0.34, -0.03, -0.42,
        0.18, -0.19, 0.08, -0.07, -0.17,
        0.19, 0.7, 0.18, 0.81, 0.17, -0.39,
        -0.9, -0.11, -0.49, 0.45, -0.06, -0.02,
        0.18, 0.07, -0.64, -0.73, -0.14, -0.28,
        0.62, -0.69, -0.72, -0.02, 0.08, -0.06,
        -0.75, 0.64, 0.34, 0.47, 0.97, -0.25, 0.28,
        0, 0, 0, 0, 0, 0),
      /* problem  3  (4190) */ Array(
        0.85, -0.36, 0.78, -0.21, 0.46, 0.84,
        -0.98, 0.2, 0.06, 0.09, -0.72, 0.94,
        -0.34, 0.56, -0.23, 0.8, -0.2, -0.1,
        0.75, -0.09, 0.65, -0.72, -0.05, 0.64,
        0.83, 0.08, 0.2, 0.16, 0.49, -0.65,
        -0.05, 0.24, -0.72, -0.51, 0.25, 0.5,
        0.17, 0.99, 0.61, -0.21, 0.31, 0.88,
        0, 0, 0, 0, 0, 0),
      /* problem  4  (3943) */ Array(
        -0.23, -0.5, 0.09, -0.34, -0.19, -0.49,
        0.21, -0.9, 0.74, 0.72, 0.12, 0.19,
        0.7, 0.69, 0.81, 0.17, -0.39, -0.9,
        -0.11, -0.49, 0.35, -0.77, -0.02, 0.18,
        -0.51, -0.64, -0.48, -0.09, -0.24, 0.18,
        -0.69, -0.72, -0.22, 0.92, -0.06, -0.75,
        0.64, -0.1, -0.29, -0.75, 0.65, -0.65,
        0, 0, 0, 0, 0, 0),
      /* problem  5  (3447) */ Array(
        -0.23, -0.5, 0.09, -0.34, -0.19, -0.49,
        0.21, -0.9, 0.74, 0.72, 0.12, 0.19,
        0.7, 0.69, 0.81, 0.17, -0.39, -0.9,
        -0.11, -0.49, 0.35, -0.77, -0.02, 0.18,
        -0.51, -0.64, -0.48, -0.09, -0.24, 0.18,
        -0.69, -0.72, -0.22, 0.92, -0.06, -0.75,
        0.64, -0.1, -0.29, -0.75, 0.65, -0.65,
        0, 0, 0, 0, 0, 0),
      /* problem  6  (6151) */ Array(
        0.23, -0.88, -0.87, 0.55, 0.15, -0.6,
        0.28, 0.47, -0.05, -0.89, -0.17, 0.93,
        -0.18, 0.04, -0.43, -0.42, -0.18, -0.38,
        -0.27, 0.13, 0.76, -0.83, 0.74, -0.54,
        0.26, 0.69, -0.4, 0.51, -0.85, 0.05,
        -0.5, 0.54, 0.02, -0.14, -0.06, -0.61,
        -0.82, 0.34, 0.91, -0.49, -0.97, 0.69,
        0, 0, 0, 0, 0, 0),
      /* problem  7  (2615) */ Array(
        -0.55, 0.8, 0.73, -0.81, 0.57, 0.96,
        0.13, 0.99, 0.09, 0.17, -0.17, -0.54,
        -0.77, -0.25, 0.79, -0.04, -0.26, -0.41,
        -0.96, 0.21, 0.95, -0.9, -0.83, -0.54,
        0.67, -0.74, 0.94, -0.4, 0.77, -0.45,
        -0.55, -0.62, -0.52, 0.09, -0.21, -0.54,
        -1.0, 0.96, -0.56, 0.41, -0.83, 0.28,
        0, 0, 0, 0, 0, 0),
      /* problem  8 (12765) */ Array(
        0.3, -0.64, 0.31, -0.36, 0.11, -0.95,
        -0.1, -0.3, -0.23, -0.78, 0.95, 0.94,
        -0.61, 0.6, 0.33, 0.13, -0.15, 0.64,
        0.53, -0.96, 0.44, -0.86, 0.31, -0.52,
        0.63, 0.36, -0.55, -0.94, -0.39, 0.04,
        0.21, -0.1, 0.28, 0.14, 0.2, -0.29,
        0.78, 0.34, 0.95, 0.74, -0.25, 0.64,
        0, 0, 0, 0, 0, 0),
      /* problem  9  (2740) */ Array(
        0.75, -0.34, -0.08, 0.8, 0.15, -0.2,
        -0.17, -0.19, -0.43, 0.59, 0.13, -0.74,
        0.31, 0.44, 0.78, 0.54, -0.51, 0.95,
        0.94, -0.11, 0.01, -0.54, -0.46, 0.67,
        -0.66, -0.49, -0.73, 0.48, 0.26, 0.6,
        0.84, 0.8, -0.79, 0.08, -0.12, -0.71,
        0.16, 0.15, 0.82, -0.08, 0.37, -0.71,
        0, 0, 0, 0, 0, 0),
      /* problem 10  (4184) */ Array(
        -0.54, -0.15, -0.45, 0.15, -0.61, -0.75,
        0.68, -0.16, -0.21, -0.56, -0.39, 0.94,
        0.04, 0.16, 1.0, -0.15, -0.81, 0.7,
        0.79, -0.31, 0.1, -0.82, 0.78, 0.9,
        -0.98, -0.5, -0.8, 0.64, -0.91, -0.82,
        -0.62, -0.86, 0.54, -0.73, 0.43, -0.71,
        -0.91, 0.41, 0.93, -0.61, -0.24, 0.82,
        0, 0, 0, 0, 0, 0),
      /* problem 11  (3180) */ Array(
        -0.68, -0.96, 0.59, 0.9, -0.66, -0.98,
        0.62, -0.88, 0.31, -0.74, -0.78, -0.19,
        0.93, 0.03, -0.43, -0.72, -0.07, 0.54,
        -0.22, 0.16, 0.73, -0.27, 0.35, 0.33,
        -0.3, -0.92, 0.92, -0.85, -0.85, -0.48,
        -0.88, -0.07, -0.88, -0.54, -0.63, 0.36,
        0.52, 0.73, -0.27, -0.75, -0.9, 0.41,
        0, 0, 0, 0, 0, 0),
      /* problem 12  (5037) */ Array(
        0.13, -0.15, 0.31, 0.82, 0.55, -0.34,
        -0.95, 0.67, 0.31, -0.39, -0.23, -0.92,
        0.39, 0.96, -0.33, 0.31, 0.16, 0.4,
        0.07, 0.63, 0.62, -0.58, -0.53, 0.95,
        0.85, -0.5, -0.97, 0.16, -0.87, 0.6,
        0.1, -0.23, 0.8, 0.42, -0.15, -0.18,
        0.79, -0.55, 0.48, -1.0, -0.59, 0.41,
        0, 0, 0, 0, 0, 0),
      /* problem 12  (6481) */ Array(
        -0.46, -0.88, -0.08, -0.68, 0.78, -0.49,
        -0.02, -0.11, 0.16, 0.59, -0.33, 0.56,
        0.62, 0.92, -0.13, -0.66, -0.51, 0.1,
        0.04, 0.36, 0.45, -0.09, 0.92, 0.27,
        -0.51, -0.38, -0.98, -0.46, 0.82, -0.34,
        0.04, -0.95, 0.65, -0.67, 0.56, -0.4,
        0.22, -0.24, 0.71, -0.91, 0.37, -0.65,
        0, 0, 0, 0, 0, 0),
      /* problem 13  (2220) */ Array(
        0.55, -0.85, 0.25, -0.05, 0.55, -0.69,
        0.7, 0.99, -0.15, -0.22, -0.87, 0.3,
        -0.43, 0.49, 0.16, -0.08, 0.35, 0.54,
        -0.95, -0.88, 0.43, -0.66, 0.74, 0.95,
        0.85, 0.39, 0.71, -0.81, -0.39, 0.18,
        0.25, -0.92, 0.52, -0.29, -0.71, -0.15,
        0.44, -0.41, 0.91, -0.05, 0.43, -0.38,
        0, 0, 0, 0, 0, 0),
      /* problem 14  (6594) */ Array(
        -0.21, 0.02, -0.83, -0.98, -0.68, 0.94,
        -0.61, 0.19, 0.34, -0.22, -0.5, -0.03,
        -0.46, 0.79, -0.46, 0.06, 0.12, 0.89,
        0.41, 0.25, 0.05, -0.75, -0.98, 0.93,
        0.4, 0.78, -0.06, -0.99, 0.32, 0.03,
        0.98, 0.97, 0.14, 0.37, -0.99, -0.58,
        -0.97, -0.24, 0.03, -0.71, 0.47, -0.53,
        0, 0, 0, 0, 0, 0),
      /* problem 15  (3526) */ Array(
        0.38, -0.88, -0.77, 0.66, -0.93, -0.79,
        0.08, -0.35, -0.53, -0.59, -0.38, -0.37,
        0.64, 0.84, 0.44, -0.1, -0.02, -0.33,
        0.91, -0.97, 0.46, -0.62, -0.85, 0.95,
        -0.51, -0.45, -0.95, 0.41, 0.36, -0.06,
        -0.18, -0.75, -0.93, 0.61, -0.06, -0.33,
        -0.48, 0.17, -0.23, 0.87, -0.97, 0.41,
        0, 0, 0, 0, 0, 0),
      /* problem 16  (6360) */ Array(
        -0.23, 0.46, 0.34, 0.38, 0.92, -0.25,
        0.35, 0.9, 0.66, 0.54, 0.62, 0.61,
        -0.51, 0.53, 0.51, -0.12, 0.16, -0.7,
        0.56, -0.54, 0.76, -0.42, 0.57, 0.7,
        0.45, -0.39, -0.6, 0.0, -0.3, 0.27,
        0.11, 0.5, -0.56, -0.28, -0.25, -0.08,
        0.64, 0.59, 0.57, 0.51, -0.77, -0.71,
        0, 0, 0, 0, 0, 0),
      /* problem 17  (5294) */ Array(
        0.02, -0.76, 0.31, -0.38, -0.38, -0.94,
        0.95, -0.26, 0.96, -0.56, -0.61, 0.73,
        -0.45, 0.63, 0.85, -0.38, -0.55, 0.74,
        0.87, 0.57, 0.05, -0.16, -0.34, 0.95,
        0.62, 0.36, 0.47, -0.46, -0.2, 0.67,
        0.54, 0.24, 0.54, -0.85, 0.34, 0.11,
        0.05, -0.47, 0.48, -0.91, 0.65, -0.31,
        0, 0, 0, 0, 0, 0),
      /* problem 18  (8314) */ Array(
        -0.46, -0.88, 0.98, -0.06, 0.25, -0.42,
        -0.74, 0.84, 0.34, -0.24, -0.01, 0.7,
        -0.46, 0.36, -0.46, 0.82, 0.12, 0.89,
        0.41, 0.36, 0.05, -0.75, -0.57, 0.34,
        0.77, -0.64, -0.06, 0.15, 0.82, -0.34,
        -0.05, -0.27, -0.78, -0.67, 0.66, 0.13,
        -0.91, -0.58, 0.93, -0.95, 0.47, -0.93,
        0, 0, 0, 0, 0, 0),
      /* problem 19  (5118) */ Array(
        -0.02, 0.48, 0.4, -0.17, -0.44, -0.55,
        0.87, 0.22, -0.28, 0.17, -0.16, -0.91,
        -0.9, 0.84, 0.44, -0.43, -0.11, -0.71,
        0.23, -0.88, 0.9, -0.72, -0.62, -0.77,
        -0.71, -0.86, -0.02, 0.67, -0.94, 0.08,
        0.23, -0.36, -0.93, -0.95, 0.66, 0.46,
        0.2, -0.23, 0.8, 0.81, 0.57, 0.91,
        0, 0, 0, 0, 0, 0),
      /* problem 20  (5688) */ Array(
        0.5, -0.15, -0.87, -0.06, 0.89, -0.35,
        0.95, 0.47, 0.76, 0.05, -0.89, 0.71,
        -0.13, 0.01, 0.05, 0.38, 0.06, 0.22,
        0.04, -0.18, 0.45, 0.32, 0.92, 0.18,
        -0.02, -0.74, 0.91, 0.28, 0.67, 0.3,
        0.72, -0.85, -0.65, -0.96, 0.75, 0.89,
        -0.4, 0.34, 0.37, 0.25, -0.31, 0.97,
        0, 0, 0, 0, 0, 0),
      /* problem 21  (1424) */ Array(
        0.14, -0.36, 0.79, -0.51, -0.44, -0.06,
        -0.02, 0.6, -0.28, -0.02, 0.49, -0.91,
        0.84, 0.84, 0.14, -0.11, 0.83, -0.39,
        -0.51, 0.41, 0.34, -0.72, 0.68, 0.7,
        -0.71, -0.01, -0.54, -0.49, -0.6, 0.12,
        -0.03, -0.36, -0.21, -0.02, 0.03, 0.46,
        0.21, -0.05, 0.8, 0.07, 0.26, -0.79,
        0, 0, 0, 0, 0, 0),
      /* problem 22  (2200) */ Array(
        -0.26, -0.15, 0.74, -0.03, 0.97, -0.52,
        0.55, -0.24, -0.51, 0.11, 0.93, 0.09,
        0.61, 0.13, 0.33, 0.54, 0.06, 0.74,
        0.88, 0.96, -0.02, -0.54, 0.58, 0.98,
        -0.36, 0.9, -0.53, -0.46, -0.63, 0.3,
        -0.22, 0.8, -0.72, 0.41, 0.66, -0.49,
        -0.25, 0.15, -0.97, -0.53, -0.38, 0.88,
        0, 0, 0, 0, 0, 0),
      /* problem 23  (1107) */ Array(
        -0.46, -0.98, 0.32, -0.36, -0.24, -0.81,
        0.45, 0.87, -0.23, -0.79, -0.8, -0.41,
        0.68, 0.53, 0.66, -0.58, -0.25, 0.41,
        0.56, -0.77, 0.58, -0.65, 0.92, 0.45,
        -0.45, -0.13, -0.46, 0.36, -0.07, 0.96,
        -0.37, -0.22, -0.22, -0.73, -0.02, -0.57,
        -0.11, 0.99, 0.48, -0.82, -0.37, 0.86,
        0, 0, 0, 0, 0, 0),

      /* meta 0 - 6 (26416 out of 29805) */ Array(
        0.02, 0.13, -0.87, -0.95, 0.44, 0.15,
        -0.85, 0.67, 0.38, -0.18, -0.09, 0.54,
        0.2, 0.33, -0.43, 0.95, 0.16, 0.87,
        0.61, 0.22, 0.73, -0.68, -0.7, 0.27,
        -0.28, 0.4, -0.73, 0.39, -0.88, -0.48,
        0.81, -0.02, 0.47, -0.99, -0.09, 0.03,
        -0.93, -0.8, 0.48, -0.21, -0.15, 0.41,
        0, 0, 0, 0, 0, 0),
      /* meta 7 -13 (23732 out of 26853) */ Array(
        0.6, -0.2, -0.1, -0.54, -0.29, -0.79,
        -0.21, -0.11, 0.46, -0.79, -0.61, 0.54,
        0.62, 0.04, 0.66, -0.21, 0.82, 0.7,
        0.8, -0.26, 0.58, -0.84, -0.02, -0.98,
        -0.51, -0.38, -0.55, -0.21, -0.39, -0.06,
        0.88, 0.57, -0.93, -0.95, -0.5, 0.99,
        0.66, 0.84, 0.8, 0.74, 0.62, 0.44,
        0, 0, 0, 0, 0, 0),
      /* meta 15-21 (22221 out of 22341) */ Array(
        -0.34, -0.88, 0.09, 0.15, 0.6, -0.21,
        0.73, 0.47, -0.96, 0.42, 0.35, -0.28,
        0.45, -0.07, 0.65, -0.43, 0.83, 0.94,
        -0.31, 0.98, 0.62, -0.16, -0.53, 0.93,
        -0.36, -0.5, 0.92, -0.09, -0.0, 0.06,
        -0.73, -0.1, -0.16, 0.42, -0.09, -0.49,
        -0.82, -0.36, 0.67, -0.49, 0.08, 0.64,
        0, 0, 0, 0, 0, 0),

      /* fast problems */ Array(
        -0.23, -0.01, -0.6, -0.93, 0.27, 0.97,
        -0.69, -0.83, 0.85, -0.42, 0.35, -0.24,
        0.38, 0.16, -0.46, 0.01, 0.58, 0.87,
        0.32, 0.98, 0.26, -0.31, -0.95, -0.7,
        -0.98, -0.44, -0.48, -0.16, -0.3, -0.91,
        0.07, -0.66, 0.88, -0.96, -0.07, -0.06,
        -0.96, 0.57, 1.0, 0.74, -0.71, 0.93,
        0, 0, 0, 0, 0, 0)
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
