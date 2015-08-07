package eu.shiftforward.icfpc2015

import java.io.File
import spray.json._
import eu.shiftforward.icfpc2015.model.{ Command, PowerPhrase, Grid, Input }
import org.specs2.mutable.Specification

import scala.io.Source

class PowerPhraseSpec extends Specification {

  "The PowerPhrase" should {
    val powerphrase1 = new PowerPhrase("ei!".toList)
    val powerphrase2 = new PowerPhrase("lae".toList)
    val powerphrase3 = new PowerPhrase("aa".toList)
    val powerphrase4 = new PowerPhrase("aeaa".toList)
    val powerphrase5 = new PowerPhrase("la".toList)

    "match power phrases in commands" in {
      val commandList: List[Command] = Command.string("olaei!ie")

      PowerPhrase._match(commandList, List(powerphrase1)) must be_==(
        List((3, powerphrase1))
      )

    }

    "match powerphrases n times in command sequentially" in {
      val commandList: List[Command] = Command.string("laelaelae")

      PowerPhrase._match(commandList, List(powerphrase2)) must be_==(
        List(
          (0, powerphrase2),
          (3, powerphrase2),
          (6, powerphrase2)
        )
      )
    }

    "match powerphrases n times in command with overlaps" in {
      val commandList: List[Command] = Command.string("aaaa")

      PowerPhrase._match(commandList, List(powerphrase3)) must be_==(
        List(
          (0, powerphrase3),
          (1, powerphrase3),
          (2, powerphrase3)
        ))
    }

    "match several powerphrases" in {
      val commandList: List[Command] = Command.string("laeaa")

      PowerPhrase._match(commandList,
        List(powerphrase1, powerphrase3, powerphrase2, powerphrase4, powerphrase5)) must be_==(
          List(
            (0, powerphrase5),
            (0, powerphrase2),
            (1, powerphrase4),
            (3, powerphrase3)
          ))
    }
  }
}
