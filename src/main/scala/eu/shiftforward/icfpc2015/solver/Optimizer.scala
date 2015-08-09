package eu.shiftforward.icfpc2015.solver

import eu.shiftforward.icfpc2015.GameState
import eu.shiftforward.icfpc2015.model.{ Grid, Input }
import spray.json._

import scala.io.Source
import scala.util.Random

case class OptimizationResult(score: Long, parameters: Array[Double])

trait Optimizer {
  val hyperparametersLenght = 36

  protected def score(filename: String, hp: Array[Double]) = {
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

    def randomPool(archetype: Specimen): Pool = {
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

    val petri = new GeneticExploration(
      0.05, 0.5, 64, // rate of mutation, crossover ratio, max population
      () => (Random.nextDouble() - 0.5) * 2, // random gene pool
      g => g + (Random.nextDouble() - 0.5) / 5, // gene mutator
      cs => cs.map(v => (math floor v * 100) / 100).toArray, // how to build a specimen from genes
      cs => { val sum = cs.sum; cs.map(_ / sum) },
      fitness, // the fitness function
      (iter, _) => iter > 20 // the stop condition
    )

    val best = petri.evolution(petri.toMatePool(petri.randomPool(Array.fill(hyperparametersLenght)(0.5))))._1.maxBy(_._2)

    println(f"DONE\tBest Fit ${best._2}\tSpecimen ${best._1.toList}")

    OptimizationResult(best._2, best._1)
  }
}

object OptimizerMain extends App {
  println(GeneticOptimizer.optimize(args, 20))
}