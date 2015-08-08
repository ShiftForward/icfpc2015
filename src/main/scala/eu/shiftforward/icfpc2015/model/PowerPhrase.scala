package eu.shiftforward.icfpc2015.model

import scala.collection.mutable

case class PowerPhrase(text: List[Char]) {
  val movements = text.map(Command.char)
  val length = text.length
}

object PowerPhrase {

  val knownPhrases = List(
    "Ei!", // from statement
    "Ia! Ia!", // from problem 3 grid
    "R'lyeh", // from problem 5 grid
    "Yuggoth") // from problem 7 grid

  def getMatchings(source: List[Command], powerphrases: List[PowerPhrase]): Map[PowerPhrase, List[Int]] = {

    var matching = List[(Int, PowerPhrase)]()
    var matched = Map[PowerPhrase, List[Int]]()

    for (i <- source.indices) {
      val command = source(i)
      powerphrases.foreach { p =>
        matching = matching :+ (0, p)
      }

      matching = matching.foldLeft(List[(Int, PowerPhrase)]()) {
        case (acc, (idx, power)) =>

          if (command.action == power.movements(idx).action) {
            if (idx + 1 == power.movements.length) {
              val startingIdx = i - power.movements.length + 1

              matched = matched.updated(power, matched.getOrElse(power, List[Int]()) :+ startingIdx)
              acc
            } else {
              acc :+ (idx + 1, power)
            }
          } else {
            acc
          }
      }
    }
    matched
  }

  def flatten(sourceLength: Int, matchings: Map[PowerPhrase, List[Int]]): Map[Int, PowerPhrase] = {

    val sortedMatches = matchings.toList.sortWith {
      case ((powerA, idxsA), (powerB, idxsB)) =>
        if (idxsA.length == idxsB.length)
          powerA.movements.length > powerB.movements.length
        else
          idxsA.length > idxsB.length
    }

    val matchingsSortedNoOverlaps = sortedMatches.map {
      case (power, idxs) =>
        val movementLength = power.movements.length

        val newIndexes = idxs.tail.foldLeft(List(idxs.head)) {
          case (acc, i) =>
            if (acc.last + movementLength <= i) {
              acc :+ i
            } else {
              acc
            }
        }

        (power, newIndexes)
    }

    var freePositions = List.fill(sourceLength)(true)
    matchingsSortedNoOverlaps.foldLeft(Map[Int, PowerPhrase]()) {
      case (acc, (power, idxs)) =>

        val succesfulPlacements = mutable.ArrayBuffer[(Int, PowerPhrase)]()

        idxs foreach { i =>
          val valid = freePositions.slice(i, i + power.length - 1).reduce(_ && _)

          if (valid) {
            succesfulPlacements += (i -> power)
            for (j <- 0 until power.length) {
              freePositions = freePositions.updated(i + j, false)
            }
          }
        }

        acc ++ succesfulPlacements.toMap
    }
  }

  def getBestString(source: List[Command], powerPhrases: List[PowerPhrase]): String = {
    val matchings = getMatchings(source, powerPhrases)
    val finalMatchings = flatten(source.length, matchings)

    var result = ""
    var i = 0
    while (i < source.length) {
      finalMatchings.get(i) match {
        case None =>
          result += source(i).ch
          i += 1
        case Some(power) =>
          result += power.text.mkString("")
          i += power.text.length
      }
    }
    result
  }
}
