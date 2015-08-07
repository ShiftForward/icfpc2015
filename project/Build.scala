import sbt._
import spray.json._
import spray.json.lenses.JsonLenses._
import spray.json.DefaultJsonProtocol._

import scala.io.Source

object ICFPCBuild extends Build {
  {
    System.setProperty("jsse.enableSNIExtension", "false")
  }

  val rank = TaskKey[Unit]("rank", "Gives the current rank position'")

  val rankTask = rank := {
    val ranksJson = Source.fromURL("https://davar.icfpcontest.org/rankings.js").mkString.drop(11).parseJson
    val ourTeamJson = ranksJson.extract[JsValue]('data / 'rankings / find('team.is[String](_ == "Guru Meditation")))

    ourTeamJson match {
      case None =>
        println("Failed to find our result.")
      case Some(json) =>
        val ourRank = json.extract[Int]('rank)
        val ourScore = json.extract[Int]('score)
        println("Rank : " + ourRank)
        println("Score: " + ourScore)
    }
  }

  lazy val project = Project(
    "project",
    file("."),
    settings = Defaults.coreDefaultSettings ++ Seq(rankTask)
  )
}