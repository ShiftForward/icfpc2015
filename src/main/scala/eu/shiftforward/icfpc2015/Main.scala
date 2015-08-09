package eu.shiftforward.icfpc2015

import eu.shiftforward.icfpc2015.model._
import eu.shiftforward.icfpc2015.model.Input._
import eu.shiftforward.icfpc2015.model.Output._
import eu.shiftforward.icfpc2015.solver._
import spray.json._
import spray.json.DefaultJsonProtocol._

import scala.io.Source

object Main extends App {

  case class Options(
    files: List[String] = List(),
    timeLimit: Option[Int] = None,
    memoryLimit: Option[Int] = None,
    coreNumber: Option[Int] = None,
    phrasesOfPower: List[PowerPhrase] = List())

  def nextOption(opts: Options, remArgs: List[String]): Options = {
    remArgs match {
      case Nil => opts
      case "-f" :: value :: tail =>
        nextOption(opts.copy(files = opts.files :+ value), tail)
      case "-t" :: value :: tail =>
        nextOption(opts.copy(timeLimit = Some(value.toInt)), tail)
      case "-m" :: value :: tail =>
        nextOption(opts.copy(memoryLimit = Some(value.toInt)), tail)
      case "-c" :: value :: tail =>
        nextOption(opts.copy(coreNumber = Some(value.toInt)), tail)
      case "-p" :: value :: tail =>
        nextOption(opts.copy(phrasesOfPower = opts.phrasesOfPower :+ PowerPhrase(value)), tail)
      case _ => nextOption(opts, remArgs.tail)
    }
  }

  val options = nextOption(Options(), args.toList)

  val solver = new SmartSolver(debugOnGameOver = false)

  val sols = options.files.par.flatMap { problemFile =>
    val input = Source.fromFile(problemFile).mkString.parseJson.convertTo[Input]

    input.sourceSeeds.par.map { seed =>
      val units = input.orderedUnitsBySeed(seed)
      val grid = Grid(input.width, input.height).filled(input.filled: _*)
      val gameState = GameState(grid, units, options.phrasesOfPower, countScore = false)

      val solution = solver.play(gameState, options.timeLimit).toList
      Output(input.id, seed, None, solution)
    }
  }.seq

  println(sols.toJson.prettyPrint)
}
