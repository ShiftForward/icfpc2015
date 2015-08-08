package eu.shiftforward.icfpc2015.solver

import eu.shiftforward.icfpc2015.GameState
import eu.shiftforward.icfpc2015.model.{ Grid, Input }
import spray.json._

import scala.io.Source
import scala.util.Random

case class OptimizationResult(score: Long, parameters: HyperParameters)

trait Optimizer {
  def score(filename: String, hp: HyperParameters) = {
    val input = Source.fromFile(filename).mkString.parseJson.convertTo[Input]
    val solver = new SmartSolver(hp, debugOnGameOver = false)
    val score = input.sourceSeeds.map { seed =>
      val units = input.orderedUnitsBySeed(seed)
      val grid = Grid(input.width, input.height).filled(input.filled: _*)
      val gameState = GameState(grid, units)
      val solution = solver.play(gameState).toList
      gameState.nextState(solution).score.currentScore
    }.sum / input.sourceSeeds.size
    OptimizationResult(score, hp)
  }

  def optimize(filename: String, maxIter: Int): OptimizationResult
}

object RandomOptimizer extends Optimizer {
  def optimize(filename: String, maxIter: Int) = {
    def optimizeAux(iter: Int, bestModel: OptimizationResult): OptimizationResult = iter match {
      case 0 => bestModel
      case i =>
        val hp = HyperParameters(
          Random.nextDouble() - 0.5, Random.nextDouble() - 0.5, Random.nextDouble() - 0.5, Random.nextDouble() - 0.5,
          Random.nextDouble() - 0.5, Random.nextDouble() - 0.5)
        val newModel = score(filename, hp)
        if (newModel.score > bestModel.score) optimizeAux(i - 1, newModel)
        else optimizeAux(i - 1, bestModel)
    }
    optimizeAux(maxIter, score(filename, HyperParameters()))
  }
}

object GeneticOptimizer extends Optimizer {
  class GeneticExploration[Gene, Specimen](val mutationRate: Double,
                                           val crossOverRate: Double,
                                           val population: Int,
                                           geneGenerator: () => Gene,
                                           specimenBuilder: Iterable[Gene] => Specimen,
                                           fitnessF: Specimen => Long,
                                           stopCondition: (Int, List[Specimen]) => Boolean)(implicit ev1: Specimen => Iterable[Gene]) {

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

      println(f"Epoch $epoch  Best Fit ${best._2}  Specimen ${best._1}")

      val newGeneration = popReproduction(pool)
      if (stopCondition(epoch, newGeneration)) (newGeneration, epoch)
      else evolution(newGeneration, epoch + 1)
    }

    private[this] def matePool(pool: Pool): MatePool = {
      val fitnesses = pool.par.map(fitnessF).toArray
      pool.zip(fitnesses)
    }

    @inline private[this] def renormalize(vector: Array[Long]): Array[Double] = {
      val sum = vector.sum
      vector.map(_.toDouble / sum)
    }

    private[this] def popReproduction(matePool: MatePool): Pool = {
      val normalizedPool = matePool.map(_._1).zip(renormalize(matePool.map(_._2).toArray))

      // Preserve the better specimen (elitist)
      (matePool.maxBy(_._2)._1 +: (1 to population).par.map(_ =>
        crossover(monteCarlo(normalizedPool), monteCarlo(normalizedPool)))).toList
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
        if (mutationRate > Random.nextFloat) geneGenerator() else gene))
  }

  def optimize(filename: String, maxIter: Int) = {
    type Specimen = HyperParameters
    type Gene = Double

    def fitness(s: Specimen): Long = score(filename, s).score

    implicit def toIterable(s: Specimen): Iterable[Gene] = Array(s.a, s.b, s.c, s.d, s.e, s.f)

    val petri = new GeneticExploration[Gene, Specimen](
      0.01, 0.25, 128, () => (Random.nextDouble() - 0.5) * 2, // rate of mutation, crossover ratio, max population and gene pool
      cs => { val x = cs.toArray; HyperParameters(x(0), x(1), x(2), x(3), x(4), x(5)) }, // how to build a specimen from genes
      fitness, // the fitness function
      (iter, _) => iter > 20 // the stop condition
    )

    val best = petri.evolution(petri.toMatePool(petri.randomPool(HyperParameters())))._1.maxBy(_._2)

    println(f"DONE\tBest Fit ${best._2}\tSpecimen ${best._1}")

    OptimizationResult(best._2, best._1)
  }
}

object OptimizerMain extends App {
  println(GeneticOptimizer.optimize(args(0), 20))
}