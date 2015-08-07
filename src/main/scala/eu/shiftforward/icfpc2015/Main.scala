package eu.shiftforward.icfpc2015

import java.io.File

import eu.shiftforward.icfpc2015.model._
import eu.shiftforward.icfpc2015.model.Input._
import spray.json._
import scala.io.Source

object Main extends App {
  val solutionTag = "test"

  val sols = new File("problems").listFiles.flatMap { problemFile =>
    val problem = Source.fromFile(problemFile).mkString.parseJson.convertTo[Input]

    problem.sourceSeeds.map { seed =>
      val commands = Command.string("Ei!") // some smart calculations here
      Output(problem.id, seed, solutionTag, commands)
    }
  }

  Client.submit(sols) match {
    case res if res.contains("created") => println("Solutions submitted successfully")
    case res => println("A problem happened: " + res)
  }
}
