package eu.shiftforward.icfpc2015

import java.io.File
import spray.json._
import eu.shiftforward.icfpc2015.model.{ CellUnit, Grid, Input }
import org.specs2.mutable.Specification

import scala.io.Source

class GameStateRendererSpec extends Specification {

  "The GameStateRender" should {

    "print all problems" in {
      forall(new File("problems").listFiles) { problemFile =>
        val input = Source.fromFile(problemFile).mkString.parseJson.convertTo[Input]
        println(problemFile.getName)
        println(GameStateRenderer.asString(
          GameState(
            Grid(input.width, input.height).filled(input.filled: _*),
            Iterator.empty
          )
        )
        )
        ok
      }
    }
  }
}
