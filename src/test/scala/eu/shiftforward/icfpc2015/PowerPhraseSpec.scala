package eu.shiftforward.icfpc2015

import eu.shiftforward.icfpc2015.model.{ Command, PowerPhrase }
import org.specs2.mutable.Specification

class PowerPhraseSpec extends Specification {

  "The PowerPhrase" should {
    val powerphrase1 = new PowerPhrase("ei!".toList)
    val powerphrase2 = new PowerPhrase("lae".toList)
    val powerphrase3 = new PowerPhrase("aa".toList)
    val powerphrase4 = new PowerPhrase("aeaa".toList)
    val powerphrase5 = new PowerPhrase("la".toList)

    "match power phrases in commands" in {
      val commandList: List[Command] = Command.string("olaei!ie")

      PowerPhrase.getMatching(commandList, List(powerphrase1)) must be_==(
        Map(powerphrase1 -> List(3))
      )

    }

    "match powerphrases n times in command sequentially" in {
      val commandList: List[Command] = Command.string("laelaelae")

      PowerPhrase.getMatching(commandList, List(powerphrase2)) must be_==(
        Map(
          powerphrase2 -> List(0, 3, 6)
        )
      )
    }

    "match powerphrases n times in command with overlaps" in {
      val commandList: List[Command] = Command.string("aaaa")

      PowerPhrase.getMatching(commandList, List(powerphrase3)) must be_==(
        Map(
          powerphrase3 -> List(0, 1, 2)
        ))
    }

    "match several powerphrases" in {
      val commandList: List[Command] = Command.string("laeaaa")

      PowerPhrase.getMatching(commandList,
        List(powerphrase1, powerphrase3, powerphrase2, powerphrase4, powerphrase5)) must be_==(
          Map(
            powerphrase2 -> List(0),
            powerphrase5 -> List(0),
            powerphrase4 -> List(1),
            powerphrase3 -> List(3, 4)
          ))
    }
  }
}
