package eu.shiftforward.icfpc2015.model

import spray.json._
import java.io.File

import org.specs2.mutable.Specification

import scala.io.Source

class InputSpec extends Specification {

  "The Input models" should {

    "have a JSON protocol that works on all problems" in {
      forall(new File("problems").listFiles) { problemFile =>
        val json = Source.fromFile(problemFile).mkString.parseJson
        json.convertTo[Input] mustEqual json.convertTo[Input].toJson.convertTo[Input]
      }
    }
  }
}
