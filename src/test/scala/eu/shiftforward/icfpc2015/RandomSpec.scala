package eu.shiftforward.icfpc2015.model

import org.specs2.mutable.Specification

import eu.shiftforward.icfpc2015.Utils

class RandomSpec extends Specification {

  "The Linear Congruential Generator" should {

    "generate the same numbers as given by the spec" in {
      (Utils.random(17) take 11 mkString ", ") mustEqual "0, 24107, 16552, 12125, 9427, 13152, 21440, 3383, 6873, 16117, 444"
    }
  }
}
