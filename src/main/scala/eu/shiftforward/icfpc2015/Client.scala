package eu.shiftforward.icfpc2015

import java.io.ByteArrayInputStream

import spray.json._
import spray.json.DefaultJsonProtocol._
import com.typesafe.config.ConfigFactory
import eu.shiftforward.icfpc2015.model.Output
import scala.sys.process._

object Client {
  val config = ConfigFactory.load
  val apiToken = config.getString("auth.api-token")
  val teamId = config.getInt("auth.team-id")

  val curlCommand = s"""curl -s -X POST
    |--user :$apiToken
    |-H Content-Type:application/json
    |-d @- https://davar.icfpcontest.org/teams/$teamId/solutions""".stripMargin.replaceAll("\n", " ")

  def submit(sols: Seq[Output]) = {
    val input = new ByteArrayInputStream(sols.toJson.compactPrint.getBytes)
    (curlCommand #< input).!!
  }
}
