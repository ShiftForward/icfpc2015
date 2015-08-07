package eu.shiftforward.icfpc2015

import eu.shiftforward.icfpc2015.model.{ Command, PowerPhrase }
import org.specs2.mutable.Specification

class PowerPhraseSpec extends Specification {

  "The PowerPhrase" should {
    val powerphrase1 = PowerPhrase("ei!".toList)
    val powerphrase2 = PowerPhrase("lae".toList)
    val powerphrase3 = PowerPhrase("aa".toList)
    val powerphrase4 = PowerPhrase("aeaa".toList)
    val powerphrase5 = PowerPhrase("la".toList)

    "match power phrases in commands" in {
      val command = Command.string("olaei!ie")

      PowerPhrase.getMatchings(command, List(powerphrase1)) must be_==(
        Map(powerphrase1 -> List(3))
      )

    }

    "match powerphrases n times in command sequentially" in {
      val command = Command.string("laelaelae")

      PowerPhrase.getMatchings(command, List(powerphrase2)) must be_==(
        Map(
          powerphrase2 -> List(0, 3, 6)
        )
      )
    }

    "match powerphrases n times in command with overlaps" in {
      val command = Command.string("aaaa")

      PowerPhrase.getMatchings(command, List(powerphrase3)) must be_==(
        Map(
          powerphrase3 -> List(0, 1, 2)
        ))
    }

    "match several powerphrases" in {
      val command = Command.string("laeaaaa")

      PowerPhrase.getMatchings(command,
        List(powerphrase1, powerphrase3, powerphrase2, powerphrase4, powerphrase5)) must be_==(
          Map(
            powerphrase2 -> List(0),
            powerphrase5 -> List(0),
            powerphrase4 -> List(1),
            powerphrase3 -> List(3, 4, 5)
          ))
    }

    "flatten in case of overlaps" in {
      // 1. 1 power phrase with inner overlaps
      val command = Command.string("aaaa")

      val matchings = PowerPhrase.getMatchings(command, List(powerphrase3))

      PowerPhrase.flatten(command.length, matchings) must beEqualTo {
        Map(0 -> powerphrase3, 2 -> powerphrase3)
      }

      // 2. Several power phrases with inner overlaps
      val command2 = Command.string("laeaaaa")
      val matchings2 = PowerPhrase.getMatchings(command2,
        List(powerphrase1, powerphrase3, powerphrase2, powerphrase4, powerphrase5))

      PowerPhrase.flatten(command2.length, matchings2) must beEqualTo {
        Map(
          0 -> powerphrase2,
          3 -> powerphrase3,
          5 -> powerphrase3
        )
      }
    }

    "convert to a string containing the most power of phrases" in {
      PowerPhrase.getBestString(Command.string("l4ea4aa"),
        List(powerphrase1, powerphrase3, powerphrase2, powerphrase4, powerphrase5)) must beEqualTo("laeaaaa")

      PowerPhrase.getBestString(Command.string("aeaaaeaa"),
        List(powerphrase1, powerphrase3, powerphrase2, powerphrase4, powerphrase5)) must beEqualTo("aeaaaeaa")
    }
  }
}
