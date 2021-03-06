package eu.shiftforward.icfpc2015.model

import spray.json._
import spray.json.DefaultJsonProtocol._

case class Output(problemId: Int,
                  seed: Int,
                  tag: Option[String],
                  solution: List[Command])

object Output {
  implicit val commandJsonFormat = lift {
    new JsonWriter[List[Command]] {
      def write(commands: List[Command]) = JsString(commands.mkString)
    }
  }

  implicit val outputJsonFormat = jsonFormat4(Output.apply)
}
