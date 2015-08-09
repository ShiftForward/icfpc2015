package eu.shiftforward.icfpc2015

import java.io.File

import eu.shiftforward.icfpc2015.model.Input._
import eu.shiftforward.icfpc2015.model._
import eu.shiftforward.icfpc2015.solver._
import spray.json._

import scala.io.Source

object Submit extends App {
  val solver: Solver = new SmartSolver()
  val solutionTag: Option[String] = None

  val cliDryRun = args.contains("--dryrun")
  val cliVerbose = args.contains("--verbose")

  val powerPhrases = PowerPhrase.knownPhrases

  val sols = new File("problems").listFiles.flatMap { problemFile =>
    val input = Source.fromFile(problemFile).mkString.parseJson.convertTo[Input]

    input.sourceSeeds.map { seed =>
      val units = input.orderedUnitsBySeed(seed)
      val grid = Grid(input.width, input.height).filled(input.filled: _*)
      val solution = solver.play(GameState(grid, units, powerPhrases)).toList

      if (cliVerbose) {
        println(s"Solution for $problemFile (seed $seed): ${solution.map(_.ch).mkString}")
      }
      Output(input.id, seed, solutionTag, solution)
    }
  }

  if (!cliDryRun) {
    Client.submit(sols) match {
      case res if res.contains("created") =>
        println(s"Solutions submitted successfully" + solutionTag.fold("")(" with tag " + _))

      case res => println(s"A problem happened: $res")
    }
  }
}
