package eu.shiftforward.icfpc2015

import java.io.ByteArrayInputStream

import spray.json._
import spray.json.DefaultJsonProtocol._
import com.typesafe.config.ConfigFactory
import eu.shiftforward.icfpc2015.model.{ Command, Output }
import scala.sys.process._

object Client {
  val config = ConfigFactory.load
  val apiToken = config.getString("auth.api-token")
  val teamId = config.getInt("auth.team-id")

  val curlCommand = s"""curl -s -X POST
    |--user :$apiToken
    |-H Content-Type:application/json
    |-d @- https://davar.icfpcontest.org/teams/$teamId/solutions""".stripMargin.replaceAll("\n", " ")

  def submit(sols: List[Output]) = {
    val input = new ByteArrayInputStream(sols.toJson.compactPrint.getBytes)
    (curlCommand #< input).!!
  }
}

object ClientExample extends App {
  val sol = (0 to 23).map { i => Output(i, 0, "test", Command.string("Ei!")) }.toList
  println(Client.submit(sol))
}
